"""Command-line interface using Typer."""

import typer
import uvicorn

from .utils.config import get_settings

app = typer.Typer(help="Emacs RAG Server CLI")


@app.command()
def serve(
    host: str = typer.Option(None, help="Host to bind to (overrides EMACS_RAG_HOST)"),
    port: int = typer.Option(None, help="Port to bind to (overrides EMACS_RAG_PORT)"),
    reload: bool = typer.Option(False, help="Enable auto-reload for development")
):
    """
    Start the FastAPI server.

    Examples:
        emacs-rag-server serve
        emacs-rag-server serve --host 0.0.0.0 --port 8080
        emacs-rag-server serve --reload
    """
    settings = get_settings()

    # Use provided values or fall back to settings
    bind_host = host or settings.host
    bind_port = port or settings.port

    typer.echo(f"Starting Emacs RAG Server on {bind_host}:{bind_port}")
    typer.echo(f"Database: {settings.db_path}/rag.db")
    typer.echo(f"Embedding model: {settings.embedding_model}")
    typer.echo(f"Reranking: {'enabled' if settings.rerank_enabled else 'disabled'}")

    uvicorn.run(
        "emacs_rag_server.main:app",
        host=bind_host,
        port=bind_port,
        reload=reload,
        log_level="info"
    )


@app.command()
def config():
    """
    Display current configuration.
    """
    settings = get_settings()

    typer.echo("=== Emacs RAG Server Configuration ===\n")

    typer.echo("[Database]")
    typer.echo(f"  Path: {settings.db_path}")
    typer.echo()

    typer.echo("[Chunking]")
    typer.echo(f"  Chunk Size: {settings.chunk_size} chars")
    typer.echo(f"  Chunk Overlap: {settings.chunk_overlap} chars")
    typer.echo()

    typer.echo("[Embedding]")
    typer.echo(f"  Model: {settings.embedding_model}")
    typer.echo(f"  Dimensions: {settings.vector_dimensions}")
    typer.echo()

    typer.echo("[Reranking]")
    typer.echo(f"  Model: {settings.rerank_model}")
    typer.echo(f"  Enabled: {settings.rerank_enabled}")
    typer.echo(f"  Top-K: {settings.rerank_top_k}")
    typer.echo()

    typer.echo("[Server]")
    typer.echo(f"  Host: {settings.host}")
    typer.echo(f"  Port: {settings.port}")


@app.command()
def init_db():
    """
    Initialize database schema.

    Creates tables if they don't exist.
    """
    from .models.database import init_schema

    settings = get_settings()
    settings.ensure_paths()

    typer.echo(f"Initializing database at {settings.db_path}/rag.db")
    init_schema()
    typer.echo("✓ Database schema initialized")


if __name__ == "__main__":
    app()
