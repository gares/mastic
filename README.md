# Mastic

Mastic is a library that makes Menhir-based parsers error-resilient.

## What is error resilience?

A parser is error-resilient if:
- It never fails to produce a semantic value.
- It never discards any token.

Compilers typically stop at the first syntax error, so standard parsing
technologies like Menhir don't prioritize resilience. In contrast,
language servers used by IDEs almost never receive an error-free
document, yet they still need to process it. If the standard parser just
fails, they can't use it. Writing and maintaining a separate, error-resilient
parser is unsatisfactory because it must be kept in sync with the main one.

To ensure a result is always produced, the syntax tree must include at least
one (top-level) node that can carry errors; therefore this node cannot be `None`.
A parser that always returns the node `Error [all the tokens]` is technically
resilient, although not very useful. Keeping all tokens is important because they
correspond to text elements the IDE manipulates, from syntax highlighting
to completion to jump-to-definition.

## What does Mastic do for me?

With minimal changes to your grammar and syntax tree, Mastic
gives you an error-resilient parser.

For the syntax tree, you add at least one `Error` node.
More fine-grained error nodes are even better. The additions follow a
predictable pattern that a ppx could generate.

For the grammar, you append the same production at the end of each block of
rules that produces the `Error` node. Again, this is systematic and a Menhir
ppx could generate it.

In addition, you can provide a grammar-specific error handler to improve recovery,
that is, to generate `Error` nodes for the smallest possible span of text.
For example, it can identify good synchronization points for restarting the
parser, such as tokens that begin top-level grammar entries.

## What is the status of this software?

EXPERIMENTAL


