# autolith-webui

A native Common Lisp web console for [Autolith](https://autolith.rocks). htmx
frontend, `hx-encoding="multipart/form-data"` uploads, in-image Lisp REPL,
and headless `autolith run-job` integration. No web frameworks: the HTTP
server is hand-rolled on usocket + bordeaux-threads + flexi-streams.

```
  AUTOLITH::CONSOLE
  [overview] [chat] [agent] [repl] [files] [state] [sessions]
```

## Features

- **Overview** — autolith version, models, detached-session count, saved
  conversations, open agenda items, memories, papercuts, server log tail.
- **Agent** — submit one contracted headless child-agent job
  (`autolith run-job`): task text, role, permissions mode, timeout,
  workspace, and an optional file attachment. Jobs run in background
  threads, poll every 3 s, and show status, result, usage tokens, and
  process output. Kill button for running jobs.
- **Chat** — per-session chat with detached sessions: a sidebar built from
  `autolith localgroup status`, a transcript rendered from the session's
  conversation event log (user/agent bubbles, collapsible tool calls), 3 s
  polling with scroll position and open-fold state preserved, and a
  composer that queues messages via `autolith localgroup tell`.
- **REPL** — evaluates in the server's Lisp image, package `WEBUI-USER`
  (uses `COMMON-LISP` and `UIOP`). `*READ-EVAL*` disabled, 20 s timeout
  (SBCL), output captured, history kept, package reset button.
- **Files** — htmx multipart uploads (`hx-encoding="multipart/form-data"`)
  with multiple-file support, byte-exact binary round-trip, inline preview
  for text/SVG, download, delete.
- **State** — read-only view of Autolith's durable state
  (`agendas.sexp`, `memories.sexp`, `papercuts.sexp`), parsed with a safe
  reader (`*READ-EVAL*` disabled, symbols interned in `KEYWORD`, custom
  `#A` dispatch for Autolith's non-simple string print syntax).
  Refreshes every 20 s.
- **Sessions** — `autolith localgroup status` table with pause/kill
  actions and a message-queue (`tell`) form. Refreshes every 12 s.

## Quickstart

```sh
git clone https://github.com/sroerick/autolith-webui ~/autolith-webui && cd ~/autolith-webui
make install-links   # symlink into ~/quicklisp/local-projects
make build           # dumps bin/autolith-webui (SBCL core, ~13 MB)
./bin/autolith-webui # http://127.0.0.1:8078
```

Run from source instead of a core dump:

```sh
make install-links
./bin/start.sh
```

Smoke test without starting the server:

```sh
make smoke
```

## Configuration (environment)

| Variable | Default | Meaning |
|---|---|---|
| `AUTOLITH_WEBUI_PORT` | `8078` | Listen port |
| `AUTOLITH_WEBUI_HOST` | `127.0.0.1` | Bind address — keep it local |
| `AUTOLITH_WEBUI_TOKEN` | unset | When set, all requests must present it via `?token=`, cookie `token`, or `Authorization: Bearer` |
| `AUTOLITH_WEBUI_HOME` | `~/.local/share/autolith-webui/` | Data directory |
| `AUTOLITH_WEBUI_UPLOADS` | `$HOME/uploads/` | Upload target directory |
| `AUTOLITH_WEBUI_JOBS` | `$HOME/jobs/` | Job envelope/result artifacts |
| `AUTOLITH_WEBUI_STATE_DIR` | `~/.local/share/autolith/` | Autolith state to render |
| `AUTOLITH_WEBUI_WORKSPACE` | `~` | Default workspace for agent jobs |
| `AUTOLITH_WEBUI_STATIC` | `<source>/static/` | Static asset directory (assets are also embedded in the dumped executable) |

## Endpoints

| Method | Path | Purpose |
|---|---|---|
| GET | `/` | Shell page (overview inlined) |
| GET | `/panel/{overview,agent,chat,repl,files,state,sessions}` | Panel fragments; without an `HX-Request` header a full page is rendered |
| POST | `/agent/run` · `/agent/kill` · GET `/agent/jobs` | Headless job lifecycle |
| GET | `/chat/threads` · `/chat/thread` · `/chat/messages` · POST `/chat/send` | Session chat: sidebar, transcript fragments, multipart send |
| POST | `/repl` · `/repl/reset` | In-image evaluation |
| POST | `/files/upload` · `/files/delete` · GET `/files/download` | Multipart files |
| GET | `/state/body` · `/sessions/body` | Polled state and session tables |
| POST | `/sessions/tell` · `/sessions/action` | localgroup control |
| GET | `/static/<file>` | Assets (disk first, embedded cache as fallback) |

htmx patterns in use: `hx-get`/`hx-post`, `hx-trigger="every Ns"` polling,
`hx-encoding="multipart/form-data"` uploads, `hx-indicator` spinners,
`hx-confirm` guards, `hx-vals` payloads, `hx-push-url` history, and
progressive-enhancement fallback for direct visits.

## Headless job contract

Submissions write a data-only S-expression and run:

```
autolith run-job --input jobs/<id>.input --output jobs/<id>.result --permissions <mode>
```

Envelope shape (exactly what `run-job` validates):

```lisp
(:autolith-job :version 1 :id "j..." :role "task"
 :prompt "..." :input (:task "..." :attachments nil :source :autolith-webui)
 :output-contract (:type :string) :timeout-seconds 900)
```

The result envelope (`:autolith-job-result`) is parsed for status, result,
failure category/message, trace id, and usage. Attachments upload via
multipart, land in the uploads directory, and their absolute paths are
handed to the job. Job roles come from Autolith's discovered task roles
(`task`, `scout`, `designer`, `reviewer`, `librarian`).

## Packaging

- `make build` — dump a compressed SBCL core executable with htmx/CSS
  embedded; serves without the source tree.
- `make package` — source tarball in `dist/`.
- `make install-links` — register with `~/quicklisp/local-projects` so
  `(asdf:load-system :autolith-webui)` works anywhere.

## Notes

- The REPL panel is a full-privilege evaluation in the server image.
  Protection is the default localhost bind plus the optional token.
- `run-job` jobs run with the permissions mode you pick; `sandbox` keeps
  commands network-isolated and read-only-host.
- The jobs table is in-memory: restarting the console clears it (input and
  result artifacts stay under the jobs directory).
- SBCL is the tested implementation (executable dump, eval timeout, signal
  handling are SBCL-conditional). The source is plain Common Lisp and runs
  on other implementations that usocket/bordeaux-threads support; a CLISP
  build skips the core dump and eval timeout.

## License

MIT — see [LICENSE](LICENSE).
