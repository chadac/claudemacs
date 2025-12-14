"""Claude Agent - Wrapper using Claude Agent SDK with dynamic permissions.

Outputs newline-delimited JSON messages for Emacs to parse.
See protocol.py for message type definitions.
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
    # Message counter for periodic reminders
    message_count: int = 0


class ClaudeAgent:
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

    def _emit(self, msg: dict) -> None:
        """Send JSON message to Emacs (stdout as NDJSON)."""
        try:
            _make_stdout_blocking()
            json_str = json.dumps(msg, ensure_ascii=False)
            print(json_str, flush=True)
            sys.stdout.flush()  # Extra flush to ensure delivery
            self._log_json("EMIT", msg)
        except (BlockingIOError, BrokenPipeError, OSError) as e:
            sys.stderr.write(f"Failed to emit: {e}\n")

    def _emit_ready(self) -> None:
        """Emit the ready message to signal ready for input."""
        self._emit({"type": "ready"})

    def _emit_session_info(self, model: Optional[str] = None, session_id: Optional[str] = None) -> None:
        """Emit session info message with model and session_id."""
        if model:
            self.state.model = model
        if session_id:
            self.state.session_id = session_id
        if model or session_id:
            self._emit({
                "type": "session_info",
                "model": model,
                "session_id": session_id,
            })

    def _emit_error(self, message: str, tb: Optional[str] = None) -> None:
        """Emit error message."""
        self._emit({
            "type": "error",
            "message": message,
            "traceback": tb,
        })

    def _emit_session_message(self, text: str) -> None:
        """Emit a session info message (for system notifications)."""
        self._emit({"type": "session_message_start"})
        self._emit({"type": "session_message_text", "text": text})
        self._emit({"type": "session_message_end"})

    def _filter_system_reminders(self, text: str) -> str:
        """Remove <system-reminder>...</system-reminder> blocks from text."""
        import re
        # Remove system-reminder blocks (including newlines around them)
        filtered = re.sub(
            r'\n?<system-reminder>.*?</system-reminder>\n?',
            '',
            text,
            flags=re.DOTALL
        )
        return filtered

    def _format_tool_args(self, tool_name: str, tool_input: dict) -> str:
        """Format tool input for display as function-style args."""
        if tool_name in ("Read", "Write", "Edit"):
            return tool_input.get("file_path", "")
        elif tool_name == "Bash":
            cmd = tool_input.get("command", "")
            # Truncate long commands
            if len(cmd) > 60:
                return cmd[:57] + "..."
            return cmd
        elif tool_name == "Glob":
            return tool_input.get("pattern", "")
        elif tool_name == "Grep":
            pattern = tool_input.get("pattern", "")
            path = tool_input.get("path", "")
            if path:
                return f"{pattern}, {path}"
            return pattern
        elif tool_name == "WebFetch":
            return tool_input.get("url", "")
        elif tool_name == "Task":
            return tool_input.get("description", "")[:40]
        else:
            # For unknown tools, show first key=value
            for k, v in tool_input.items():
                val = str(v)[:40]
                return f"{k}={val}"
            return ""

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
        self._emit({
            "type": "permission_request",
            "tool_name": tool_name,
            "tool_input": tool_input,
        })

        # Wait for user response via stdin
        self._permission_event = asyncio.Event()
        self._permission_response = None

        try:
            # Wait for permission response (with timeout)
            await asyncio.wait_for(self._permission_event.wait(), timeout=300.0)
        except asyncio.TimeoutError:
            self._emit_session_message("Permission request timed out")
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
                self._emit({
                    "type": "permission_granted",
                    "pattern": pattern,
                    "scope": "always",
                })
            elif action == "allow_session":
                self.state.session_permissions.add(pattern)
                self._emit({
                    "type": "permission_granted",
                    "pattern": pattern,
                    "scope": "session",
                })
            elif action == "allow_once":
                self._emit({
                    "type": "permission_granted",
                    "pattern": pattern,
                    "scope": "once",
                })

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
            self._emit_error(f"Failed to save permission: {e}")

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

        # Echo the user message back
        self._emit({"type": "user_start"})
        self._emit({"type": "user_text", "text": message})
        self._emit({"type": "user_end"})

        # Add org-mode reminder periodically (every 10 messages, starting with first)
        self.state.message_count += 1
        if self.state.message_count % 10 == 1:
            full_message = f"{message}\n\n(Reminder: Your response will be displayed in an Emacs buffer with org-mode formatting. Always use org-mode syntax, NOT markdown. Use *bold* not **bold**, use /italic/ not *italic*, use =code= not `code`, use #+begin_src/#+end_src not ```)"
        else:
            full_message = message

        self.state.status = "thinking"
        self._emit({"type": "thinking", "status": "Thinking..."})

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

                if msg_type == "SystemMessage":
                    # Init message with model and session info
                    data = getattr(msg, "data", {})
                    model = data.get("model")
                    session_id = data.get("session_id")
                    if model or session_id:
                        self._emit_session_info(model=model, session_id=session_id)

                    # Emit MCP server status if available
                    mcp_servers = data.get("mcp_servers", [])
                    if mcp_servers:
                        self._emit({
                            "type": "mcp_status",
                            "servers": mcp_servers,
                        })

                elif msg_type == "AssistantMessage":
                    # Extract model if available
                    model = getattr(msg, "model", None)
                    if model and model != self.state.model:
                        self._emit_session_info(model=model)

                    # Handle content blocks in the assistant message
                    for block in getattr(msg, "content", []):
                        block_type = type(block).__name__

                        if block_type == "TextBlock":
                            text = getattr(block, "text", "")
                            if text:
                                if not in_assistant_block:
                                    self._emit({"type": "assistant_start"})
                                    in_assistant_block = True
                                self._emit({"type": "assistant_text", "text": text})

                        elif block_type == "ToolUseBlock":
                            if in_assistant_block:
                                self._emit({"type": "assistant_end"})
                                in_assistant_block = False
                            tool_name = getattr(block, "name", "unknown")
                            tool_input = getattr(block, "input", {})
                            current_tool = tool_name

                            # Special handling for Edit tool - emit diff info
                            if tool_name == "Edit":
                                self._emit({
                                    "type": "edit_tool",
                                    "file_path": tool_input.get("file_path", ""),
                                    "old_string": tool_input.get("old_string", ""),
                                    "new_string": tool_input.get("new_string", ""),
                                })
                            # Special handling for Write tool - emit content for diff
                            elif tool_name == "Write":
                                self._emit({
                                    "type": "write_tool",
                                    "file_path": tool_input.get("file_path", ""),
                                    "content": tool_input.get("content", ""),
                                })
                            else:
                                # Emit tool call
                                self._emit({
                                    "type": "tool_call",
                                    "name": tool_name,
                                    "input": tool_input,
                                })
                                # Also emit thinking status for tool execution
                                tool_args = self._format_tool_args(tool_name, tool_input)
                                self._emit({
                                    "type": "thinking",
                                    "status": f"Running: {tool_name}({tool_args})",
                                })

                    # Update token counts if available
                    usage = getattr(msg, "usage", None)
                    if usage:
                        total_output_tokens = getattr(usage, "output_tokens", 0)
                        total_input_tokens = getattr(usage, "input_tokens", 0)
                        self._emit({
                            "type": "progress",
                            "input_tokens": total_input_tokens,
                            "output_tokens": total_output_tokens,
                        })

                elif msg_type == "UserMessage":
                    # UserMessage contains tool results
                    for block in getattr(msg, "content", []):
                        block_type = type(block).__name__

                        if block_type == "ToolResultBlock":
                            # Emit tool result content
                            content = getattr(block, "content", None)
                            is_error = getattr(block, "is_error", False)
                            result_text = ""
                            if content:
                                # Content can be a string or list of content blocks
                                if isinstance(content, str):
                                    result_text = self._filter_system_reminders(content)
                                elif isinstance(content, list):
                                    parts = []
                                    for item in content:
                                        if hasattr(item, "text"):
                                            parts.append(self._filter_system_reminders(item.text))
                                        elif isinstance(item, dict) and "text" in item:
                                            parts.append(self._filter_system_reminders(item["text"]))
                                        elif isinstance(item, str):
                                            parts.append(self._filter_system_reminders(item))
                                    result_text = "\n".join(parts)

                            self._emit({
                                "type": "tool_result",
                                "content": result_text,
                                "is_error": is_error,
                            })
                            # Close the tool
                            if current_tool:
                                self._emit({"type": "tool_end"})
                                current_tool = None

                elif msg_type == "ResultMessage":
                    # Conversation turn complete
                    if in_assistant_block:
                        self._emit({"type": "assistant_end"})
                        in_assistant_block = False
                    if current_tool:
                        self._emit({"type": "tool_end"})
                        current_tool = None

                    # Get cost and session info
                    cost = getattr(msg, "total_cost_usd", None) or getattr(msg, "cost_usd", 0) or 0
                    session_id = getattr(msg, "session_id", None)

                    # Emit final stats
                    self._emit({
                        "type": "result",
                        "cost_usd": cost,
                        "duration_ms": getattr(msg, "duration_ms", 0) or 0,
                        "num_turns": getattr(msg, "num_turns", 0) or 0,
                        "total_input": total_input_tokens,
                        "total_output": total_output_tokens,
                    })
                    # ResultMessage signals end of turn - break out of loop
                    self._log_json("DEBUG", {"action": "breaking out of receive_messages loop"})
                    break

        except Exception as e:
            self._emit_error(str(e), _format_traceback())

        self.state.status = "ready"
        self._emit_ready()
        # Small yield to ensure output is flushed before returning to event loop
        await asyncio.sleep(0.01)

    async def interrupt(self) -> None:
        """Interrupt the current Claude operation."""
        if self._client:
            try:
                await self._client.interrupt()
            except Exception:
                pass
        self._emit_session_message("Interrupted")
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
    agent = ClaudeAgent(
        work_dir=work_dir,
        mcp_config=mcp_config,
        allowed_tools=allowed_tools,
        log_file=log_file,
        resume_session=resume_session,
        continue_session=continue_session,
    )

    # Show initial session info
    agent._emit({"type": "session_start", "work_dir": work_dir})
    agent._emit_session_message(f"Session started in {work_dir}")
    agent._emit_session_message("Using Claude Agent SDK with dynamic permissions")
    agent._emit_ready()

    # Read input from stdin - expect NDJSON messages
    loop = asyncio.get_event_loop()
    reader = asyncio.StreamReader()
    protocol = asyncio.StreamReaderProtocol(reader)
    await loop.connect_read_pipe(lambda: protocol, sys.stdin)

    # Queue for user messages (so we can handle permissions while processing)
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

                # Parse JSON message
                try:
                    msg = json.loads(text)
                except json.JSONDecodeError:
                    agent._emit_error(f"Invalid JSON: {text}")
                    continue

                msg_type = msg.get("type")

                if msg_type == "quit":
                    await agent.quit()
                    break
                elif msg_type == "interrupt":
                    await agent.interrupt()
                elif msg_type == "permission_response":
                    # Permission response - handle immediately (unblocks can_use_tool)
                    agent.handle_permission_response(msg)
                elif msg_type == "message":
                    # Queue user messages for processing
                    await message_queue.put(msg.get("text", ""))
                else:
                    agent._emit_error(f"Unknown message type: {msg_type}")

            except Exception as e:
                agent._emit_error(f"stdin error: {e}")

    # Task to process user messages to Claude
    async def process_messages():
        while agent._running:
            try:
                text = await asyncio.wait_for(message_queue.get(), timeout=1.0)
                await agent.send_user_message(text)
            except asyncio.TimeoutError:
                continue
            except Exception as e:
                agent._emit_error(f"Error: {e}")
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
        description="Claude Agent - Using Claude Agent SDK"
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
