# `claude`

Haskell bindings to Anthropic's Claude API using `servant`.

## Setup

### Using Nix with Flakes (Recommended)

1. Ensure you have Nix with flakes enabled
2. Copy the sample environment file:

```bash
cp .envrc.sample .envrc
```

3. Edit `.envrc` with your Anthropic API key
4. Run `direnv allow`

### Manual Setup

```bash
cabal build
```

## Environment Variables

```bash
export ANTHROPIC_KEY="your-anthropic-api-key"
```
