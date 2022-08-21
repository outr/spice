# spice
A new framework for server and client HTTP communication. Intentional speed and simplicity with a
core focus on OpenAPI / Swagger support.

## 1.0
- [X] Net Foundation
  - [X] Port + compile-time interpolation
  - [X] IP address + compile-time interpolation
  - [X] Path support + compile-time interpolation
  - [X] URL support + compile-time interpolation
  - [X] Fabric JSON encoding/decoding support for all types
  - [X] ContentType implementation
- [X] Stream support
  - [X] Porting of IO from YouI into Stream
  - [X] Migration from blocking to cats-effect
- [ ] HTTP Foundation
  - [X] Migration of YouI HTTP classes
  - [ ] Cleanup and updates to representations
  - [ ] Fabric JSON encoding/decoding support for request and response
- [ ] Extraction into multi-platform
  - [ ] Cross-platform code
  - [ ] Scala.js
  - [ ] ScalaNative considerations?
- [ ] HTTP Client support
  - [ ] JVM implementation using okhttp
  - [ ] JS implementation using AJAX
  - [ ] ScalaNative?
  - [ ] Testing
- [ ] HTTP Server support
  - [ ] Moduload for implementation details
  - [ ] JVM implementation using Undertow
  - [ ] Migration from YouI
  - [ ] Updates to using cats-effect and fs2
  - [ ] OpenAPI-focused DSL for end-points
- [ ] Preliminary benchmarks