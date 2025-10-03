"""Text chunking utilities."""

from typing import Iterable, List, Tuple


def chunk_text(text: str, *, chunk_size: int, overlap: int = 0) -> List[Tuple[str, int]]:
    """
    Split text into overlapping chunks with line number tracking.

    Args:
        text: Input text to chunk
        chunk_size: Maximum characters per chunk
        overlap: Characters to overlap between chunks

    Returns:
        List of (chunk_text, line_number) tuples
        line_number is 1-based starting line for chunk

    Algorithm:
        1. Start at position 0
        2. Extract chunk [start:start+chunk_size]
        3. Calculate line number by counting '\n' before start
        4. Move start forward by (chunk_size - overlap)
        5. Repeat until end of text
    """
    chunks = []
    start = 0

    while start < len(text):
        end = min(start + chunk_size, len(text))
        chunk_content = text[start:end]

        # Calculate 1-based line number
        line_number = text[:start].count('\n') + 1

        chunks.append((chunk_content, line_number))

        # If we've reached the end, break
        if end == len(text):
            break

        # Move start forward (ensuring we don't go backwards)
        start = max(start + 1, end - overlap)

    return chunks


def batched(iterable: Iterable[str], batch_size: int) -> Iterable[List[str]]:
    """
    Yield batches from iterable.

    Args:
        iterable: Input iterable
        batch_size: Number of items per batch

    Yields:
        Lists of items with size up to batch_size
    """
    batch = []
    for item in iterable:
        batch.append(item)
        if len(batch) >= batch_size:
            yield batch
            batch = []
    if batch:
        yield batch
