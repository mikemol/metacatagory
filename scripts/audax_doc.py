from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, List


class ListLike:
    def __init__(self, items: Iterable):
        self._items = list(items)

    def items(self) -> List:
        return self._items


class AUDAXInline:
    def render(self) -> str:
        raise NotImplementedError


@dataclass
class Str(AUDAXInline):
    value: str

    def render(self) -> str:
        return self.value


@dataclass
class Emph(AUDAXInline):
    items: ListLike

    def render(self) -> str:
        return "*" + "".join(item.render() for item in self.items.items()) + "*"


@dataclass
class Strong(AUDAXInline):
    items: ListLike

    def render(self) -> str:
        return "**" + "".join(item.render() for item in self.items.items()) + "**"


@dataclass
class Code(AUDAXInline):
    value: str

    def render(self) -> str:
        return "`" + self.value + "`"


@dataclass
class Link(AUDAXInline):
    text: ListLike
    url: str

    def render(self) -> str:
        rendered = "".join(item.render() for item in self.text.items())
        return f"[{rendered}]({self.url})"


@dataclass
class Image(AUDAXInline):
    text: ListLike
    url: str

    def render(self) -> str:
        rendered = "".join(item.render() for item in self.text.items())
        return f"![{rendered}]({self.url})"


class Space(AUDAXInline):
    def render(self) -> str:
        return " "


class Break(AUDAXInline):
    def render(self) -> str:
        return "\n"


class AUDAXBlock:
    pass


@dataclass
class Para(AUDAXBlock):
    inlines: ListLike


@dataclass
class Header(AUDAXBlock):
    level: int
    inlines: ListLike


@dataclass
class CodeBlock(AUDAXBlock):
    code: str


@dataclass
class BlockQuote(AUDAXBlock):
    blocks: ListLike


@dataclass
class ListBlock(AUDAXBlock):
    items: ListLike  # ListLike of ListLike[AUDAXBlock]


@dataclass
class Table(AUDAXBlock):
    header: List[str]
    rows: List[ListLike]


@dataclass
class Field(AUDAXBlock):
    key: str
    value: str


@dataclass
class Raw(AUDAXBlock):
    value: str


class Null(AUDAXBlock):
    pass


@dataclass
class AUDAXDoc:
    blocks: ListLike


def render_inline(item: AUDAXInline) -> str:
    return item.render()


def render_block(block: AUDAXBlock) -> str:
    if isinstance(block, Para):
        return "".join(render_inline(i) for i in block.inlines.items())
    if isinstance(block, Header):
        level = min(max(block.level, 1), 6)
        content = "".join(render_inline(i) for i in block.inlines.items())
        return f'{"#" * level} {content}'
    if isinstance(block, CodeBlock):
        return f"```\n{block.code}\n```"
    if isinstance(block, BlockQuote):
        return "\n".join("> " + line for line in render_doc(AUDAXDoc(blocks=block.blocks)).splitlines())
    if isinstance(block, ListBlock):
        lines = []
        for item in block.items.items():
            prefix = "- "
            rendered = render_doc(AUDAXDoc(blocks=item))
            lines.append(prefix + rendered.replace("\n", "\n  "))
        return "\n".join(lines)
    if isinstance(block, Table):
        header_line = "| " + " | ".join(block.header) + " |"
        separator = "| " + " | ".join("---" for _ in block.header) + " |"
        rows = []
        for row in block.rows:
            rendered = " | ".join(render_inline(cell) for cell in row.items())
            rows.append(f"| {rendered} |")
        return "\n".join([header_line, separator] + rows)
    if isinstance(block, Field):
        return f"{block.key}: {block.value}"
    if isinstance(block, Raw):
        return block.value
    if isinstance(block, Null):
        return ""
    raise ValueError(f"Unhandled block type: {type(block)}")


def render_doc(doc: AUDAXDoc) -> str:
    rendered = []
    for block in doc.blocks.items():
        if isinstance(block, Para):
            rendered.append(render_block(block) + "\n")
        elif isinstance(block, Field):
            rendered.append(render_block(block) + "\n")
        elif isinstance(block, Table):
            rendered.append(render_block(block) + "\n")
        elif isinstance(block, Header):
            rendered.append(render_block(block) + "\n")
        else:
            rendered.append(render_block(block))
    return "\n".join(line.rstrip() for line in rendered).strip() + "\n"
