#!/usr/bin/env python3
"""Shared I/O utilities for file operations.

Provides unified JSON and Markdown I/O with consistent error handling,
path management, and encoding.
"""

import json
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union


def load_json(
    path: Union[str, Path],
    default: Optional[Any] = None,
    error_msg: Optional[str] = None,
    required: bool = False
) -> Any:
    """Load JSON file with error handling.
    
    Args:
        path: Path to JSON file
        default: Value to return if file doesn't exist (None means raise error)
        error_msg: Custom error message to display
        required: If True and file doesn't exist, exit with error
        
    Returns:
        Loaded JSON data or default value
        
    Raises:
        FileNotFoundError: If file doesn't exist and no default provided
        json.JSONDecodeError: If JSON is malformed
    """
    path = Path(path)
    
    if not path.exists():
        if default is not None:
            return default
        if required:
            msg = error_msg or f"Required file not found: {path}"
            print(f"Error: {msg}", file=sys.stderr)
            sys.exit(1)
        raise FileNotFoundError(f"File not found: {path}")
    
    try:
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)
    except json.JSONDecodeError as e:
        msg = error_msg or f"Invalid JSON in {path}: {e}"
        print(f"Error: {msg}", file=sys.stderr)
        if required:
            sys.exit(1)
        raise


def load_json_text(
    text: str,
    default: Optional[Any] = None,
    error_msg: Optional[str] = None,
    required: bool = False,
    log_errors: bool = True,
) -> Any:
    """Parse JSON from a string with consistent error handling."""
    try:
        return json.loads(text)
    except json.JSONDecodeError as e:
        msg = error_msg or f"Invalid JSON input: {e}"
        if log_errors:
            print(f"Error: {msg}", file=sys.stderr)
        if default is not None:
            return default
        if required:
            sys.exit(1)
        raise


def try_load_json(path: Union[str, Path]) -> Tuple[Optional[Any], str]:
    """Load JSON and return (data, state) for soft-failure contexts."""
    path = Path(path)
    try:
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f), "ok"
    except FileNotFoundError:
        return None, "missing"
    except json.JSONDecodeError:
        return None, "invalid"


def try_load_json_text(text: str) -> Tuple[Optional[Any], str]:
    """Parse JSON and return (data, state) for soft-failure contexts."""
    try:
        return json.loads(text), "ok"
    except json.JSONDecodeError:
        return None, "invalid"


def save_json(
    path: Union[str, Path],
    data: Any,
    indent: int = 2,
    ensure_ascii: bool = True,
    default: Any | None = None,
    create_parents: bool = True,
    encoding: str = 'utf-8'
) -> None:
    """Save data to JSON file with automatic directory creation.
    
    Args:
        path: Path to JSON file
        data: Data to serialize
        indent: JSON indentation level
        create_parents: Whether to create parent directories
        encoding: Output encoding
    """
    path = Path(path)
    
    if create_parents:
        try:
            path.parent.mkdir(parents=True, exist_ok=True)
        except FileExistsError:
            if path.parent.is_file():
                path.parent.unlink()
                path.parent.mkdir(parents=True, exist_ok=True)
            else:
                raise
    
    with open(path, 'w', encoding=encoding) as f:
        if default is None:
            json.dump(data, f, indent=indent, ensure_ascii=ensure_ascii)
        else:
            json.dump(
                data,
                f,
                indent=indent,
                ensure_ascii=ensure_ascii,
                default=default,
            )


def load_markdown(
    path: Union[str, Path],
    default: Optional[str] = None,
    required: bool = False
) -> str:
    """Load Markdown file with error handling.
    
    Args:
        path: Path to Markdown file
        default: Value to return if file doesn't exist
        required: If True and file doesn't exist, exit with error
        
    Returns:
        File contents as string
        
    Raises:
        FileNotFoundError: If file doesn't exist and no default provided
    """
    path = Path(path)
    
    if not path.exists():
        if default is not None:
            return default
        if required:
            print(f"Error: Required file not found: {path}", file=sys.stderr)
            sys.exit(1)
        raise FileNotFoundError(f"File not found: {path}")
    
    with open(path, 'r', encoding='utf-8') as f:
        return f.read()


def save_markdown(
    path: Union[str, Path],
    content: Union[str, List[str]],
    create_parents: bool = True
) -> None:
    """Save Markdown content to file.
    
    Args:
        path: Path to Markdown file
        content: Content as string or list of lines
        create_parents: Whether to create parent directories
    """
    path = Path(path)
    
    if create_parents:
        path.parent.mkdir(parents=True, exist_ok=True)
    
    if isinstance(content, list):
        content = '\n'.join(content)
    
    with open(path, 'w', encoding='utf-8') as f:
        f.write(content)


def ensure_file_exists(
    path: Union[str, Path],
    error_msg: Optional[str] = None,
    suggestion: Optional[str] = None
) -> bool:
    """Check if file exists and provide helpful error message if not.
    
    Args:
        path: Path to check
        error_msg: Custom error message
        suggestion: Suggestion for how to create the file (e.g., "Run 'make planning-index'")
        
    Returns:
        True if file exists
        
    Prints error and returns False if file doesn't exist.
    """
    path = Path(path)
    
    if not path.exists():
        msg = error_msg or f"File not found: {path}"
        print(f"Error: {msg}", file=sys.stderr)
        if suggestion:
            print(f"  Suggestion: {suggestion}", file=sys.stderr)
        return False
    
    return True


def create_directory(
    path: Union[str, Path],
    parents: bool = True,
    exist_ok: bool = True
) -> Path:
    """Create directory with sensible defaults.
    
    Args:
        path: Path to create
        parents: Create parent directories
        exist_ok: Don't error if directory exists
        
    Returns:
        Path object for created directory
    """
    path = Path(path)
    path.mkdir(parents=parents, exist_ok=exist_ok)
    return path
