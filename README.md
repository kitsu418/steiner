# Steiner

See how a language model's answer could grow, token by token, as **worldlines** you can explore in the browser.
(A nod to *Steins;Gate*: every token the model could have written is a worldline it did not take.)

At every token, the model had about twenty choices, each with a probability. **Grow** follows the most probable one,
step by step. Pick any other candidate, even a 0.1% one, and Grow again: the text branches there into a second
worldline. On the left, the worldlines form a tree; on the right, the selected one reads as the full text, with each
token's probability under it.

The prompt and the text are editable, and so is the model. Insert an `assert()`, rewrite a line, change the prompt or
pick another model: growing then starts a **new worldline** next to the place it diverged from, so you can compare how
the answer goes on with and without your change, or how two models go on from the same text.

## What you need

- OCaml 5 (5.4 is used) with dune, and these opam packages:

  ```sh
  opam install . --deps-only
  ```

- One or more OpenAI-compatible chat completions endpoints served by **vLLM**. Steiner reads next-token probabilities
  from vLLM's `prompt_logprobs` and `return_token_ids` extensions, which OpenAI's, Anthropic's and most hosted APIs do
  not offer. Purdue GenAI Studio (`https://genai.rcac.purdue.edu/api/chat/completions`) works, with `qwen3.8:27b`;
  so does your own `vllm serve` (give it `--max-logprobs` at least the width you use).

## Start

```sh
dune build
dune exec steiner -- --prompt-file examples/prompt.txt
```

Open http://127.0.0.1:8800, then **Providers** → **Add provider**: a name, the URL, a model or two, **Save**, then
paste the API key and **Set key**. **Test** scores an empty answer once, to check the key and that the server is
vLLM-backed.

| Option | Default | Meaning |
|---|---|---|
| `--providers` | `~/.config/steiner/providers.json` | the providers and models; created empty on the first start |
| `--keys` | `~/.config/steiner/keys/` | API keys, one file per provider (mode 600) |
| `--data` | `worldlines.json` | where the worldlines are kept; delete it to start over |
| `--prompt-file`, `--prefix-file` | none | the prompt, and the start of the answer, that the page opens with |
| `--host`, `--port` | 127.0.0.1, 8800 | where the page is served |
| `--page` | built in | serve this HTML file instead of the built-in page (for editing it without rebuilding) |

If the server runs on a remote machine, forward the port: `ssh -L 8800:127.0.0.1:8800 host`.

## Providers and keys

`providers.json` is edited on the page, or by hand while Steiner is stopped:

```json
{
  "providers": [
    {
      "name": "genai",
      "url": "https://genai.rcac.purdue.edu/openai/chat/completions",
      "key_env": null,
      "key_file": null,
      "min_interval": 3.5,
      "width": 20,
      "models": [
        { "name": "qwen3.8:27b", "backend": "qwen", "system": null, "vocab_file": null },
        { "name": "gpt-oss:120b", "backend": "gpt-oss", "system": null, "vocab_file": null }
      ]
    }
  ]
}
```

- `min_interval`: seconds between requests to this provider, shared by its models (GenAI Studio allows 20 a minute, so
  3.5). Use 0 for your own server.
- `width`: candidates per node (top-k).
- `backend`: the model's chat template, `qwen` (ChatML) or `gpt-oss` (harmony); see below.
- `system`: a system message sent before the prompt, instead of the backend's.
- `vocab_file`: a tiktoken vocabulary, to spell byte-piece tokens; see below.

A provider's key is never in this file. It is read, on every request, from the environment variable `key_env` when
that is set, else from `key_file`, else from `<keys dir>/<provider name>`. The page writes keys there but never reads
one back: it sees only whether a key is set and its last four characters. To set a key without the page:

```sh
mkdir -p ~/.config/steiner/keys && chmod 700 ~/.config/steiner/keys
printf '%s\n' 'sk-...' > ~/.config/steiner/keys/genai && chmod 600 ~/.config/steiner/keys/genai
```

Renaming a provider on the page moves its key file and its worldlines along. Removing one keeps its worldlines: they
can be viewed but not grown.

Because the page can change providers and keys, the API only answers requests that name this server in `Host`
(against DNS rebinding), and writes must be JSON from this page's own origin. Steiner listens on 127.0.0.1 by default;
with `--host 0.0.0.0` anyone who can reach the port can change the providers.

## Using the page

**The tree (left).** Each card is a run of text up to the next place where it branches. The first line under the text
gives its first token's probability and rank there (`0.2% · #2` means the worldline branched off with the model's 2nd
choice); `open ⋯` means it can go on, `ended` that the model finished its answer, `3 ways →` that it splits in three.
Hover a card for its full text and a plain summary. Drag to pan, scroll to zoom, **Fit** to see it all.

- **Click a card** to switch to that worldline. The candidates for the token after it hang off the card as dashed
  chips (how many: **Candidates**, bottom left). Click a chip to grow a new worldline from it.
- **Click a token in the text** to open its own position the same way: the card splits there, and the candidates that
  could have been written instead hang open.

**The text (right).** The prompt, always editable, then the selected worldline from its start, line breaks and all.
Tokens are told apart by their shading; probabilities below 90% are written under them. The token where this worldline
branched off is marked. **Edit text** turns it into a text box.

**Growing (bottom right).** Pick the model, how many tokens, and **Grow** (from the selected token, or from the end of
the worldline), **Next** (only the candidates for the next token, one request) or **Stop**. If the prompt, the text or
the model differs from the worldline's, the button reads **Grow new worldline**. Errors and retries appear below.
**Fast** writes many tokens per request instead of one; see below.

The line between the panels can be dragged. **Providers** and the theme are top right.

## Scripting it

The page drives a small HTTP API, and a script can use the same API. For example, a second model can insert asserts
into a partly generated program, as `examples/insert_asserts.py` does.

| Request | Body / query | Returns |
|---|---|---|
| `POST /api/grow` | `{"node": id or null, "prompt": str, "text": str, "steps": int, "provider"?: str, "model"?: str, "fast"?: bool}` | `{"job": j, "start": id}` |
| `GET /api/state?from=n` | | nodes from index `n` on, the jobs, the request count |
| `GET /api/node?id=i` | | that node's prompt, text, model, token, probability and children |
| `POST /api/stop` | | the state |
| `GET /api/config` | | the starting prompt and text, and the configured models |
| `GET /api/providers` | | the providers, with each key's status (never the key) |
| `PUT /api/providers` | `{"providers": [...]}`, each optionally with `"was": old name` | the providers |
| `PUT /api/providers/<name>/key` | `{"key": str}` (`""` removes it) | the providers |
| `POST /api/providers/<name>/test` | `{"model": str}` | `{"ok", "ms", "top"}` or `{"ok": false, "error"}` |

Write requests need `Content-Type: application/json`.

`POST /api/grow` decides where to start:
- **The model:** `provider` and `model` if given, else the node's own, else (no node) the first configured model.
- **Unedited:** if the prompt, the text and the model are exactly the node's own, it grows from that node.
- **Edited:** otherwise it grows from a root holding `text` with that model, with the node as its origin when the
  prompt is the same.
- **`steps` 0:** requests only the next tokens.

A job's `head` is the node it reached. Its `status` is one of `running`, `done`, `ended` (the model closed its answer),
`stopped`, or `failed` (with `error` saying why).

## How the probabilities are obtained

Expanding a node is one request. The server receives the conversation with the node's text as the assistant's
answer, `max_tokens: 1`, and `prompt_logprobs: k`. vLLM then returns, for every position of the prompt, the top-k
tokens the model would have put there. The position where the chat template closes the answer holds the model's
distribution for the next token after the text. These are the model's raw probabilities, at temperature 1 with no
sampling penalties.

The server's tokens for the text must be exactly the path's own tokens: a root's text as the server first tokenized it,
then the tokens of the nodes down to the selected one. So every probability in a worldline is the one its model gives
on that path. When the server cannot score a text as written, it is sent again with a short probe appended (`" 0"` or
`"0"`), and the distribution is read at the probe's position, which depends only on the tokens before it. This
happens in two cases:
- the template trims trailing whitespace (Qwen's does);
- the tokenizer splits text differently depending on what follows (N spaces before a number become N-1 spaces and
  `" "`).

If no probe helps, the node is marked in red and cannot be grown. A token that is only part of a character, such as a
piece of an emoji or a CJK character, cannot be grown either.

### Fast growing

With **Fast** (`"fast": true`), one request generates many tokens greedily, each with its top-k candidates
(`logprobs`/`top_logprobs`, and vLLM's `continue_final_message` so that the model goes on with the text). Before each
such request the server's tokens for the text are checked against the path, as when scoring; where they differ (Qwen's
template trims trailing whitespace) or the server refuses the request, growing falls back to scoring token by token. A
new worldline's first step is always scored.

The text is the same either way. The probabilities are not exactly: a server reports them differently while
generating than while scoring a prompt. On `qwen3.8:27b` at GenAI Studio, 40 tokens grown both ways gave the same
tokens, but candidate probabilities differed by up to 10 percentage points and the top 5 were in the same order at 22
of 40 positions. Leave Fast off when the probabilities matter. The server does not return ids for generated tokens, so
they are filled in from the next request (which returns the ids of the text it was sent).

### Backends

| | `gpt-oss` | `qwen` |
|---|---|---|
| chat template | harmony: the answer is the `final` channel message | ChatML |
| system message | a ChatGPT header with `Reasoning: low` and the channel list | none |
| answer closed by | `<|end|>`; the model ends with `<|return|>` | `<|im_end|>` |
| probes | rarely needed | needed for every text that ends in whitespace |

vLLM sends a token that is only part of a character (a byte piece) with no text. For gpt-oss these can be spelled from
the `o200k_base` vocabulary: download it and give it as the model's `vocab_file`.

```sh
curl -o ~/.config/steiner/o200k_base.tiktoken https://openaipublic.blob.core.windows.net/encodings/o200k_base.tiktoken
```

Without one, a text with byte pieces (emoji, CJK) cannot be checked. Another model family needs its own entry in
`src/backend.ml`: its system message and the token that closes the assistant's text. A Llama 3 entry, for example,
would use `<|eot_id|>`.

## Layout

| | |
|---|---|
| `bin/main.ml` | command line, HTTPS client, server start |
| `src/api.ml` | the HTTP API and its request checks |
| `src/worldlines.ml` | the forest, its file, and the grow in progress (an Eio fiber that Stop cancels) |
| `src/model.ml` | requests to a provider: rate limit, retries, probes, reading the distribution |
| `src/providers.ml` | the providers file and the keys |
| `src/backend.ml`, `src/vocab.ml` | chat templates; tiktoken vocabularies |
| `web/page.html` | the page (tree, text, providers), built into the binary |
| `test/fake_vllm.ml` | a stand-in vLLM server for testing without a model, both scoring and fast growing: `dune exec test/fake_vllm.exe -- --port 8901` |
