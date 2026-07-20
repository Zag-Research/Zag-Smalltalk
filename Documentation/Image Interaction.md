This is Samra's thesis work to extend Zed so we can get (ultimately) Pharo equivalent development environment tied into Zed's unique capabilities.

## File System Providers
![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAACO0lEQVQ4T62TsWsUURDGb+9O9wrP00jExticNsZCiChcmqBISJPCTksLwX9AUqkoiAjWSkhhlUqioFdZGDABGwULGw+00EIw6Hkab727XX/f5s3yjOtBIAsfM2929pv5Zt4GhW1+AsdXxD4B05v5g2AjJUkSOYl8xThnltAPsA/0jPAxhxPg/lYbhvwM3zQocBD7yQhV+Rq4vkXCGadsAvsq7d4RxI5MhPJTeSbNK+LH7dujvH9rOWmQD+k4sQ5FqJkOe9TZUyCZH/3EPMmSr/g8uJhV3lhEthji415nQb1e39lqtaI8QutQdhk890hPQzrJ2WZmBcJqtbqr0+ms/W+GkuzPtVAqlWYGg4GuVraAWq22t91ufyW2u1KpjHS73Q/DOsw2Xy6Xp/v9fnOTzEIYhoejKHpHfD84AN4MIzSldjXG3AK0CPkrFJqk0Aud8cfk/yOZjcfMybY8S/ISuAAWXQVJ1lV5AM6Bh+AIqINmXodXeaH7KLJHYNV99B3bcfL2YG8Ui8VLcRzr7zrGjMeZ8WLeUtSIydRi9J9q8F8cvmGfgQUwB26BCQiPQzhvhIcIKrGtBfCiiWxfplP7tyH3NnO7QrQBToK7RphmeldDG47ALwd1Kbk/wbpA7jqFFb8MppDfQP7NjJCEWRI0s/PAFpDbWU7wLN1O0e2cEY6S9Bm8BPpHBx56+P0cKEfxGLJTkL3Hv2eEOzisgdAlieS3ky3pJl+xrs5cL/k9pPZQ9hr/Dkh/9m19/gD/jfQV9a0xVAAAAABJRU5ErkJggg==)Zed does not support custom File System Providers (like those found in VS Code) for creating virtual filesystems. Zed natively handles the local disk or remote environments, so for custom file sources, you can either implement a **Language Server (LSP)** or an **Agent/MCP Server**. [1](https://www.youtube.com/watch?v=K1K84PSgp5g&t=62), [2](https://zed.dev/extensions/ini), [3](https://blog.logrocket.com/exploring-zed-open-source-code-editor-rust/), [5](https://zed.dev/blog/copilot)

The Alternative: Use MCP Servers or LSP

Since you cannot inject a custom `FileSystemProvider`, your best alternatives within Zed's extension ecosystem are:

1. **Model Context Protocol (MCP) Server:** Build an MCP server that exposes your custom file system as a resource. Zed's agent can read, write, and index these files through the MCP framework.
2. **Language Server Protocol (LSP):** Build a dummy language server that responds to `textDocument/documentSymbol` or custom Workspace Initialization options. This lets Zed traverse and open your virtual paths as if they are part of a standard project. [1](https://www.youtube.com/watch?v=wXJl4IhIYVg), [2](https://zed.dev/languages/rust), [3](https://zed.dev/extensions), [4](https://zed.dev/docs/languages/rust), [5](https://www.youtube.com/watch?v=K1K84PSgp5g&t=62), [6](https://zed.dev/extensions/ini)

How to Implement the Alternative

Option A: Building an MCP Server

Zed integrates with standard MCP servers. [1](https://www.youtube.com/watch?v=K1K84PSgp5g&t=62)

1. Write an MCP server in your language of choice (usually Python or Rust) that implements the `tools/call` and `resources/list` handlers.
2. Have the server serve your custom file contents and directory trees.
3. Open Zed, go to your Agent Panel settings, and add your custom MCP server configuration. [1](https://www.youtube.com/watch?v=K1K84PSgp5g&t=62), [2](https://zed.dev/acp/agent/claude-agent), [3](https://zed.dev/languages/rust), [4](https://skywork.ai/skypage/en/A-Deep-Dive-into-Zed-MCP-Server-The-AI-Engineer's-Guide/1972827997712609280), [5](https://www.youtube.com/watch?v=wXJl4IhIYVg)

Option B: Building an LSP Extension for Zed

If you want deep editor integration (like sidebar tree views or custom code actions):

1. **Create an Extension:** Zed extensions are written in Rust and compiled to WebAssembly. You use the Rust zed crate API.
2. **Implement a Language Server:** Have your extension launch or wrap an LSP binary that intercepts workspace/document requests for your custom protocol (e.g., `virtualfs://`).
3. **Use Dev Extensions:** To test locally, use `zed: install dev extension` from the command palette and point to your project directory. [1](https://zed.dev/blog/zed-decoded-extensions), [2](https://zed.dev/docs/extensions/developing-extensions), [3](https://zed.dev/extensions/ini), [4](https://zed.dev/blog/zed-decoded-extensions), [5](https://github.com/zed-industries/zed/issues/10906)

Refer to the [Developing Extensions in Zed](https://zed.dev/docs/extensions/developing-extensions) documentation for full instructions on structuring a local extension. , [2](https://zed.dev/docs/extensions/developing-extensions)
