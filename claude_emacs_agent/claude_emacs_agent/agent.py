"""Claude Emacs Agent - Wrapper using Claude Agent SDK with dynamic permissions.

Outputs structured markers that Emacs can parse to format the conversation nicely:
- [USER] ... [/USER] for echoing user input
- [ASSISTANT] ... [/ASSISTANT] for Claude's responses
- [TOOL name] ... [/TOOL] for tool calls
- [STATUS model cost] for status updates
- [READY] to signal ready for input
- [ERROR] ... [/ERROR] for errors
"""

import asyncio
import io
import json
import os
import sys
import traceback
from dataclasses import dataclass, field
from typing import Optional, Any
import argparse

from claude_agent_sdk import (
    ClaudeSDKClient,
    ClaudeAgentOptions,
    ToolPermissionContext,
    PermissionResultAllow,
    PermissionResultDeny,
)


# Markers for Emacs to parse
MARKER_USER_START = "[USER]"
MARKER_USER_END = "[/USER]"
MARKER_ASSISTANT_START = "[ASSISTANT]"
MARKER_ASSISTANT_END = "[/ASSISTANT]"
MARKER_TOOL_START = "[TOOL "  # followed by tool name and ]
MARKER_TOOL_END = "[/TOOL]"
MARKER_READY = "[READY]"
MARKER_ERROR_START = "[ERROR]"
MARKER_ERROR_END = "[/ERROR]"
MARKER_SESSION_START = "[SESSION]"
MARKER_SESSION_END = "[/SESSION]"
MARKER_THINKING = "[THINKING]"  # Signals Claude is processing
MARKER_PROGRESS = "[PROGRESS "  # followed by JSON and ]
MARKER_RESULT = "[RESULT "  # followed by JSON and ]
MARKER_PERMISSION_REQUEST = "[PERMISSION_REQUEST "  # followed by JSON and ]


def _format_traceback() -> str:
    """Format the current exception traceback as a string."""
    return traceback.format_exc()


def _make_stdout_blocking() -> None:
    """Ensure stdout is in blocking mode to avoid write errors."""
    try:
        import fcntl
        flags = fcntl.fcntl(sys.stdout.fileno(), fcntl.F_GETFL)
        fcntl.fcntl(sys.stdout.fileno(), fcntl.F_SETFL, flags & ~os.O_NONBLOCK)
    except (io.UnsupportedOperation, AttributeError, OSError):
        pass


@dataclass
class AgentState:
    """Current state of the agent session."""

    session_id: Optional[str] = None
    model: Optional[str] = None
    status: str = "initializing"
    pending_tool_calls: dict = field(default_factory=dict)
    # Permission tracking
    session_permissions: set = field(default_factory=set)  # Patterns allowed for session
    always_permissions: set = field(default_factory=set)  # Patterns always allowed


class ClaudeEmacsAgent:
    """Wrapper that bridges Emacs and Claude using the SDK.

    Uses claude-agent-sdk for programmatic access with dynamic permission
    handling via the can_use_tool callback.
    """

    def __init__(
        self,
        work_dir: str,
        mcp_config: Optional[str] = None,
        allowed_tools: Optional[list[str]] = None,
        log_file: Optional[str] = None,
        resume_session: Optional[str] = None,
        continue_session: bool = False,
    ):
        self.work_dir = work_dir
        self.mcp_config = mcp_config
        self.allowed_tools = allowed_tools or []
        self.state = AgentState()
        self._running = True
        self._log_file = log_file
        self._log_handle = None
        self._resume_session = resume_session
        self._continue_session = continue_session
        self._first_message = True  # Track if this is the first message
        if log_file:
            os.makedirs(os.path.dirname(log_file), exist_ok=True)
            self._log_handle = open(log_file, "w")

        # SDK client - persistent across conversation turns
        self._client: Optional[ClaudeSDKClient] = None

        # Permission handling - async event for waiting on user response
        self._permission_event: Optional[asyncio.Event] = None
        self._permission_response: Optional[dict] = None
        self._pending_permission_request: Optional[dict] = None

        # Load existing permissions from settings
        self._load_permissions()

    def _load_permissions(self) -> None:
        """Load allowed permissions from .claude/settings.local.json."""
        settings_path = os.path.join(self.work_dir, ".claude", "settings.local.json")
        try:
            if os.path.exists(settings_path):
                with open(settings_path, "r") as f:
                    settings = json.load(f)
                    allow_list = settings.get("permissions", {}).get("allow", [])
                    self.state.always_permissions = set(allow_list)
        except Exception:
            pass

    def _log_json(self, direction: str, msg: dict) -> None:
        """Log JSON message to file if logging is enabled."""
        if self._log_handle:
            import datetime
            timestamp = datetime.datetime.now().isoformat()
            self._log_handle.write(f"[{timestamp}] {direction}: {json.dumps(msg)}\n")
            self._log_handle.flush()

    def _emit(self, message: str) -> None:
        """Send message to Emacs (stdout)."""
        try:
            _make_stdout_blocking()
            print(message, flush=True)
        except (BlockingIOError, BrokenPipeError, OSError) as e:
            sys.stderr.write(f"Failed to emit: {e}\n")

    def _emit_ready(self) -> None:
        """Emit the ready marker to signal ready for input."""
        self._emit(MARKER_READY)

    def _matches_permission(self, tool_name: str, tool_input: dict) -> bool:
        """Check if this tool use matches any allowed permission pattern."""
        all_permissions = self.state.always_permissions | self.state.session_permissions

        for pattern in all_permissions:
            if self._pattern_matches(pattern, tool_name, tool_input):
                return True
        return False

    def _pattern_matches(self, pattern: str, tool_name: str, tool_input: dict) -> bool:
        """Check if a permission pattern matches the tool use."""
        # Simple pattern matching
        # Patterns can be:
        #   "Read" - match all Read calls
        #   "Read(/path/to/file)" - match exact file
        #   "Read(/path/*)" - match files in directory
        #   "Bash(echo:*)" - match bash commands starting with echo

        if pattern == tool_name:
            return True

        if not pattern.startswith(tool_name + "("):
            return False

        # Extract the pattern content
        pattern_content = pattern[len(tool_name) + 1:-1]  # Remove "ToolName(" and ")"

        # Get the relevant input value based on tool type
        if tool_name in ("Read", "Write", "Edit"):
            input_value = tool_input.get("file_path", "")
        elif tool_name == "Bash":
            input_value = tool_input.get("command", "")
        elif tool_name == "Glob":
            input_value = tool_input.get("pattern", "")
        elif tool_name == "Grep":
            input_value = tool_input.get("pattern", "")
        elif tool_name == "WebFetch":
            # Check domain pattern
            url = tool_input.get("url", "")
            if pattern_content.startswith("domain:"):
                domain = pattern_content[7:]  # Remove "domain:"
                return domain in url
            input_value = url
        else:
            # For unknown tools, try to match any input value
            input_value = str(tool_input)

        # Handle wildcards
        if pattern_content.endswith("*"):
            prefix = pattern_content[:-1]
            return input_value.startswith(prefix)

        # Handle exact match
        return pattern_content == input_value

    async def _can_use_tool(
        self,
        tool_name: str,
        tool_input: dict[str, Any],
        context: ToolPermissionContext,
    ) -> PermissionResultAllow | PermissionResultDeny:
        """Callback for permission checks. Asks user if not pre-approved."""
        self._log_json("PERMISSION_CHECK", {"tool": tool_name, "input": tool_input})

        # Check if already permitted
        if self._matches_permission(tool_name, tool_input):
            self._log_json("PERMISSION_AUTO_ALLOW", {"tool": tool_name})
            return PermissionResultAllow()

        # Need to ask user - emit permission request
        self._pending_permission_request = {
            "tool_name": tool_name,
            "tool_input": tool_input,
        }
        self._emit(f"{MARKER_PERMISSION_REQUEST}{json.dumps(self._pending_permission_request)}]")

        # Wait for user response via stdin
        self._permission_event = asyncio.Event()
        self._permission_response = None

        try:
            # Wait for permission response (with timeout)
            await asyncio.wait_for(self._permission_event.wait(), timeout=300.0)
        except asyncio.TimeoutError:
            self._emit(f"{MARKER_SESSION_START}")
            self._emit("Permission request timed out")
            self._emit(f"{MARKER_SESSION_END}")
            return PermissionResultDeny()

        response = self._permission_response
        self._permission_response = None
        self._permission_event = None
        self._pending_permission_request = None

        if not response:
            return PermissionResultDeny()

        action = response.get("action")
        pattern = response.get("pattern")

        if action == "deny":
            return PermissionResultDeny()

        # Add to appropriate permission set
        if pattern:
            if action == "allow_always":
                self.state.always_permissions.add(pattern)
                self._save_permission(pattern)
                self._emit(f"{MARKER_SESSION_START}")
                self._emit(f"Added permanent permission: {pattern}")
                self._emit(f"{MARKER_SESSION_END}")
            elif action == "allow_session":
                self.state.session_permissions.add(pattern)
                self._emit(f"{MARKER_SESSION_START}")
                self._emit(f"Added session permission: {pattern}")
                self._emit(f"{MARKER_SESSION_END}")
            # allow_once doesn't add to any set, just allows this one use

        return PermissionResultAllow()

    def _save_permission(self, pattern: str) -> None:
        """Save a permission pattern to .claude/settings.local.json."""
        settings_path = os.path.join(self.work_dir, ".claude", "settings.local.json")
        try:
            settings = {}
            if os.path.exists(settings_path):
                with open(settings_path, "r") as f:
                    settings = json.load(f)

            if "permissions" not in settings:
                settings["permissions"] = {}
            if "allow" not in settings["permissions"]:
                settings["permissions"]["allow"] = []

            if pattern not in settings["permissions"]["allow"]:
                settings["permissions"]["allow"].append(pattern)

                os.makedirs(os.path.dirname(settings_path), exist_ok=True)

                with open(settings_path, "w") as f:
                    json.dump(settings, f, indent=2)
        except Exception as e:
            self._emit(f"{MARKER_ERROR_START}")
            self._emit(f"Failed to save permission: {e}")
            self._emit(f"{MARKER_ERROR_END}")

    def handle_permission_response(self, response: dict) -> None:
        """Handle permission response from Emacs (called from stdin reader)."""
        self._permission_response = response
        if self._permission_event:
            self._permission_event.set()

    async def connect(self) -> None:
        """Initialize and connect the SDK client."""
        mcp_servers = {}
        if self.mcp_config:
            mcp_servers = self.mcp_config  # Path to config file

        options = ClaudeAgentOptions(
            cwd=self.work_dir,
            can_use_tool=self._can_use_tool,
            mcp_servers=mcp_servers if mcp_servers else {},
            allowed_tools=self.allowed_tools if self.allowed_tools else [],
            resume=self._resume_session,
            continue_conversation=self._continue_session or (not self._resume_session),
        )

        self._client = ClaudeSDKClient(options=options)
        await self._client.connect()

    async def send_user_message(self, message: str) -> None:
        """Send a user message to Claude and stream the response."""
        # Ensure client is connected
        if not self._client:
            await self.connect()

        # Echo the user message back with markers
        self._emit(f"{MARKER_USER_START}")
        self._emit(message)
        self._emit(f"{MARKER_USER_END}")

        # Add org-mode reminder to message
        full_message = f"{message}\n\n(Reminder: format your response in org-mode.)"

        self.state.status = "thinking"
        self._emit(MARKER_THINKING)

        try:
            # Send the query
            await self._client.query(full_message)

            # Stream the response
            total_output_tokens = 0
            total_input_tokens = 0
            in_assistant_block = False
            current_tool = None

            async for msg in self._client.receive_messages():
                self._log_json("RECV", {"type": type(msg).__name__, "data": str(msg)[:200]})
                msg_type = type(msg).__name__

                if msg_type == "AssistantMessage":
                    # Handle content blocks in the assistant message
                    for block in getattr(msg, "content", []):
                        block_type = type(block).__name__

                        if block_type == "TextBlock":
                            text = getattr(block, "text", "")
                            if text:
                                if not in_assistant_block:
                                    self._emit(f"{MARKER_ASSISTANT_START}")
                                    in_assistant_block = True
                                self._emit(text)

                        elif block_type == "ToolUseBlock":
                            if in_assistant_block:
                                self._emit(f"{MARKER_ASSISTANT_END}")
                                in_assistant_block = False
                            tool_name = getattr(block, "name", "unknown")
                            current_tool = tool_name
                            self._emit(f"{MARKER_TOOL_START}{tool_name}]")

                        elif block_type == "ToolResultBlock":
                            if current_tool:
                                self._emit(f"{MARKER_TOOL_END}")
                                current_tool = None

                    # Update token counts if available
                    usage = getattr(msg, "usage", None)
                    if usage:
                        total_output_tokens = getattr(usage, "output_tokens", 0)
                        total_input_tokens = getattr(usage, "input_tokens", 0)
                        progress = {
                            "output_tokens": total_output_tokens,
                            "input_tokens": total_input_tokens,
                        }
                        self._emit(f"{MARKER_PROGRESS}{json.dumps(progress)}]")

                elif msg_type == "ResultMessage":
                    # Conversation turn complete
                    if in_assistant_block:
                        self._emit(f"{MARKER_ASSISTANT_END}")
                        in_assistant_block = False
                    if current_tool:
                        self._emit(f"{MARKER_TOOL_END}")
                        current_tool = None

                    # Emit final stats
                    result_info = {
                        "cost_usd": getattr(msg, "cost_usd", 0) or 0,
                        "duration_ms": getattr(msg, "duration_ms", 0) or 0,
                        "num_turns": getattr(msg, "num_turns", 0) or 0,
                        "total_output": total_output_tokens,
                        "total_input": total_input_tokens,
                    }
                    self._emit(f"{MARKER_RESULT}{json.dumps(result_info)}]")

        except Exception as e:
            self._emit(f"{MARKER_ERROR_START}")
            self._emit(f"Error: {e}")
            self._emit(f"{MARKER_ERROR_END}")
            self._log_json("ERROR", {"error": str(e), "traceback": _format_traceback()})

        self.state.status = "ready"
        self._emit_ready()

    async def interrupt(self) -> None:
        """Interrupt the current Claude operation."""
        if self._client:
            try:
                await self._client.interrupt()
            except Exception:
                pass
        self._emit(f"{MARKER_SESSION_START}")
        self._emit("Interrupted")
        self._emit(f"{MARKER_SESSION_END}")
        self._emit_ready()

    async def quit(self) -> None:
        """Gracefully shutdown the agent."""
        self._running = False
        if self._client:
            try:
                await self._client.disconnect()
            except Exception:
                pass
        if self._log_handle:
            self._log_handle.close()


async def run_agent(
    work_dir: str,
    mcp_config: Optional[str] = None,
    resume_session: Optional[str] = None,
    continue_session: bool = False,
    allowed_tools: Optional[list[str]] = None,
    log_file: Optional[str] = None,
) -> None:
    """Run the agent, reading commands from stdin."""
    agent = ClaudeEmacsAgent(
        work_dir=work_dir,
        mcp_config=mcp_config,
        allowed_tools=allowed_tools,
        log_file=log_file,
        resume_session=resume_session,
        continue_session=continue_session,
    )

    # Show initial session info
    agent._emit(f"{MARKER_SESSION_START}")
    agent._emit(f"Session started in {work_dir}")
    agent._emit(f"Using Claude Agent SDK with dynamic permissions")
    agent._emit(f"{MARKER_SESSION_END}")
    agent._emit_ready()

    # Read input from stdin
    loop = asyncio.get_event_loop()
    reader = asyncio.StreamReader()
    protocol = asyncio.StreamReaderProtocol(reader)
    await loop.connect_read_pipe(lambda: protocol, sys.stdin)

    # Queue for user messages (so we can handle /permit while processing)
    message_queue: asyncio.Queue[str] = asyncio.Queue()

    # Task to read stdin and dispatch commands
    async def read_stdin():
        while agent._running:
            try:
                line = await reader.readline()
                if not line:
                    break

                text = line.decode().strip()
                if not text:
                    continue

                # Handle special commands immediately
                if text == "/quit":
                    await agent.quit()
                    break
                elif text == "/interrupt":
                    await agent.interrupt()
                elif text.startswith("/permit "):
                    # Permission response - handle immediately (unblocks can_use_tool)
                    try:
                        json_str = text[8:]  # Strip "/permit "
                        response = json.loads(json_str)
                        agent.handle_permission_response(response)
                    except json.JSONDecodeError as e:
                        agent._emit(f"{MARKER_ERROR_START}")
                        agent._emit(f"Invalid permission response: {e}")
                        agent._emit(f"{MARKER_ERROR_END}")
                else:
                    # Queue regular messages for processing
                    await message_queue.put(text)

            except Exception as e:
                agent._emit(f"{MARKER_ERROR_START}")
                agent._emit(f"stdin error: {e}")
                agent._emit(f"{MARKER_ERROR_END}")

    # Task to process user messages to Claude
    async def process_messages():
        while agent._running:
            try:
                text = await asyncio.wait_for(message_queue.get(), timeout=1.0)
                await agent.send_user_message(text)
            except asyncio.TimeoutError:
                continue
            except Exception as e:
                agent._emit(f"{MARKER_ERROR_START}")
                agent._emit(f"Error: {e}")
                agent._emit(f"{MARKER_ERROR_END}")
                agent._emit_ready()

    # Run both tasks concurrently
    stdin_task = asyncio.create_task(read_stdin())
    message_task = asyncio.create_task(process_messages())

    # Wait for stdin to close (quit command or EOF)
    await stdin_task
    message_task.cancel()
    try:
        await message_task
    except asyncio.CancelledError:
        pass

    await agent.quit()


def main() -> None:
    """Entry point for the agent."""
    _make_stdout_blocking()

    parser = argparse.ArgumentParser(
        description="Claude Emacs Agent - Using Claude Agent SDK"
    )
    parser.add_argument(
        "--work-dir",
        required=True,
        help="Working directory for Claude session",
    )
    parser.add_argument(
        "--mcp-config",
        default=None,
        help="Path to MCP configuration file",
    )
    parser.add_argument(
        "--resume",
        default=None,
        help="Resume session by ID (not used with SDK)",
    )
    parser.add_argument(
        "--continue",
        dest="continue_session",
        action="store_true",
        help="Continue most recent session",
    )
    parser.add_argument(
        "--allowed-tools",
        default=None,
        help="Comma-separated list of allowed tools",
    )
    parser.add_argument(
        "--log-file",
        default=None,
        help="Path to write JSON message log (for debugging)",
    )
    args = parser.parse_args()

    allowed_tools = None
    if args.allowed_tools:
        allowed_tools = [t.strip() for t in args.allowed_tools.split(",")]

    asyncio.run(
        run_agent(
            work_dir=args.work_dir,
            mcp_config=args.mcp_config,
            resume_session=args.resume,
            continue_session=args.continue_session,
            allowed_tools=allowed_tools,
            log_file=args.log_file,
        )
    )


if __name__ == "__main__":
    main()
