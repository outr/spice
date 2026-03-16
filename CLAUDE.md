# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Spice is a Scala 3 HTTP framework for server and client communication with a core focus on OpenAPI/Swagger support. It is a multi-module SBT project with Scala.js cross-compilation for core and client modules.

## Build Commands

```bash
sbt compile                    # Compile all modules
sbt +compile                   # Compile all modules for all platforms (JS + JVM)
sbt test                       # Run all tests
sbt +test                      # Run all tests for all platforms
sbt "coreJVM/testOnly spec.URLSpec"   # Run a single test class in a module
sbt coreJVM/test               # Run tests for a specific module
sbt serverImplementationUndertow/test  # Run Undertow server tests
```

Tests use ScalaTest (`AnyWordSpec` with `Matchers`). Server and OpenAPI tests run forked. All test groups run in separate subprocesses.

## Module Dependency Graph

```
core (cross JS/JVM) — foundational HTTP types, net primitives, content system
├── client (cross JS/JVM) — HTTP client abstraction
│   ├── clientImplementationOkHttp (JVM) — OkHttp3 backend
│   ├── clientImplementationJVM (JVM) — java.net.http backend
│   └── clientImplementationNetty (JVM) — Netty backend
├── delta (JVM) — HTML streaming/delta updates, parsing
├── server (JVM, depends on core + delta) — server abstractions, DSL routing
│   └── serverImplementationUndertow (JVM) — Undertow backend
└── openAPI (JVM, depends on server) — OpenAPI spec generation, typed service calls
```

## Key Packages

- `spice.net` — URL, IP, Protocol, ContentType, EmailAddress, Port (with compile-time literal validation via `literally`)
- `spice.http` — HttpRequest, HttpResponse, HttpExchange, Headers, HttpMethod, HttpStatus, Cookie
- `spice.http.content` — Content trait and implementations (StringContent, JsonContent, URLContent, FormDataContent, StreamContent)
- `spice.http.client` — HttpClientInstance, HttpClientImplementation, Interceptor, RetryManager, Proxy
- `spice.http.server` — StaticHttpServer, MutableHttpServer, ErrorHandler, BasePath
- `spice.http.server.dsl` — Filter-based routing DSL (PathFilter, ActionFilter, ConditionalFilter, ClassLoaderPath)
- `spice.openapi` — OpenAPIHttpServer, RestService, TypedServiceCall, OpenAPI spec model
- `spice.delta` — HTMLParser, HTMLStream, StreamAction, Selector

## Key Libraries

- **rapid** (`rapid-core`) — Async runtime providing `Task` and `Stream` (used throughout instead of Future/IO)
- **fabric** — JSON parsing/serialization
- **profig** — Configuration
- **scribe** — Logging
- **reactify** — Reactive variables and channels
- **moduload** — Runtime module discovery

## Architecture Notes

- Cross-platform modules use `crossProject(JSPlatform, JVMPlatform)` with platform-specific source directories
- HTTP client/server backends follow a strategy pattern — abstract interfaces in core modules, concrete implementations in separate sub-projects
- The server DSL uses composable filters: requests flow through a chain of `ConnectionFilter` instances for path matching, IP filtering, and action handling
- OpenAPI integration is type-safe: services define typed request/response pairs that auto-generate OpenAPI 3.0.3 specs (served at `/openapi.json` and `/openapi.yaml`)
- `Content` is the central abstraction for HTTP bodies — all content types implement this trait with streaming support
- Scala version: 3.8.1, Java target: 11
