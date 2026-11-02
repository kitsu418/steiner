"""Grow a program partway, have a second model insert assert() calls and comments into it, and grow on from the edited
text, all through Steiner's HTTP API. The edited text becomes a new worldline next to the node it diverged from, so
the page shows both continuations side by side.

  dune exec bin/main.exe -- --prompt-file examples/prompt.txt   (in another terminal)
  python examples/insert_asserts.py --prompt-file examples/prompt.txt --steps 120

The second model is called through the same kind of chat completions URL (an ordinary generation, no logprobs needed).
"""
import argparse
import json
import os
from pathlib import Path
import re
import time
from urllib.request import Request, urlopen

EDIT = """Below is the beginning of a program that is still being written; it stops in the middle.
Insert assert() calls that check what must hold at each point, and short comments that say what the code does.
Change nothing else, and stop exactly where the program stops: return only the edited code, with no Markdown fences.

{text}"""


def call(url, data=None, headers=None):
    request = Request(url, json.dumps(data).encode() if data is not None else None,
                      {'Content-Type': 'application/json', **(headers or {})})
    with urlopen(request, timeout=600) as response:
        return json.loads(response.read())


def grow(explorer, node, prompt, text, steps):
    """Grow and wait; returns the node the grow ended at."""
    job = call(f'{explorer}/api/grow', dict(node=node, prompt=prompt, text=text, steps=steps))['job']
    while (state := call(f'{explorer}/api/state?from=999999999'))['jobs'][job]['status'] == 'running':
        time.sleep(2)
    print(f"grow: {state['jobs'][job]['status']} after {state['jobs'][job]['added']} tokens {state['jobs'][job]['error']}")
    return state['jobs'][job]['head']


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--explorer', default='http://127.0.0.1:8800')
    parser.add_argument('--prompt-file', type=Path, required=True)
    parser.add_argument('--steps', type=int, default=120, help='tokens to grow before and after the edit')
    parser.add_argument('--url', default='https://genai.rcac.purdue.edu/openai/chat/completions', help='for the second model')
    parser.add_argument('--model', default='gpt-oss:120b', help='the second model, which inserts the asserts')
    parser.add_argument('--api-key-env', default='OPENAI_API_KEY')
    args = parser.parse_args()
    prompt = args.prompt_file.read_text()

    head = grow(args.explorer, None, prompt, '', args.steps)
    text = call(f'{args.explorer}/api/node?id={head}')['text']
    key = os.environ.get(args.api_key_env, '')
    answer = call(args.url, dict(model=args.model, messages=[{'role': 'user', 'content': EDIT.format(text=text)}]),
                  {'Authorization': f'Bearer {key}'} if key else {})['choices'][0]['message']['content']
    edited = re.sub(r'^```\w*\n|\n?```\s*$', '', answer.strip())
    print(f'--- text after {args.steps} tokens ---\n{text}\n--- edited by {args.model} ---\n{edited}')
    head = grow(args.explorer, head, prompt, edited, args.steps)  # a new worldline, drawn next to the node it diverged from
    print(f"--- grown on ---\n{call(f'{args.explorer}/api/node?id={head}')['text']}")


if __name__ == '__main__':
    main()
