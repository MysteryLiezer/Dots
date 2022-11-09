import asynchat
import asyncore
import socket
from typing import Any, Text

_Address = tuple[str, int]  # (host, port)

class SMTPChannel(asynchat.async_chat):
    COMMAND: int
    DATA: int
    def __init__(self, server: SMTPServer, conn: socket.socket, addr: Any, data_size_limit: int = ...) -> None: ...
    # base asynchat.async_chat.push() accepts bytes
    def push(self, msg: Text) -> None: ...  # type: ignore
    def collect_incoming_data(self, data: bytes) -> None: ...
    def found_terminator(self) -> None: ...
    def smtp_HELO(self, arg: str) -> None: ...
    def smtp_NOOP(self, arg: str) -> None: ...
    def smtp_QUIT(self, arg: str) -> None: ...
    def smtp_MAIL(self, arg: str) -> None: ...
    def smtp_RCPT(self, arg: str) -> None: ...
    def smtp_RSET(self, arg: str) -> None: ...
    def smtp_DATA(self, arg: str) -> None: ...

class SMTPServer(asyncore.dispatcher):
    channel_class: type[SMTPChannel]

    data_size_limit: int
    enable_SMTPUTF8: bool
    def __init__(self, localaddr: _Address, remoteaddr: _Address, data_size_limit: int = ...) -> None: ...
    def handle_accepted(self, conn: socket.socket, addr: Any) -> None: ...
    def process_message(
        self, peer: _Address, mailfrom: str, rcpttos: list[Text], data: bytes | str, **kwargs: Any
    ) -> str | None: ...

class DebuggingServer(SMTPServer): ...

class PureProxy(SMTPServer):
    def process_message(  # type: ignore
        self, peer: _Address, mailfrom: str, rcpttos: list[Text], data: bytes | str
    ) -> str | None: ...

class MailmanProxy(PureProxy):
    def process_message(  # type: ignore
        self, peer: _Address, mailfrom: str, rcpttos: list[Text], data: bytes | str
    ) -> str | None: ...