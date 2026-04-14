---
name: add-feed
description: Add an RSS/Atom feed URL to the elfeed configuration in config.org
user-invocable: true
allowed-tools: Read, Edit, Bash, Grep
argument-hint: "<feed-url> [tag]"
---

# Add Elfeed Feed

Add the feed URL `$ARGUMENTS` to the elfeed configuration.

## Steps

1. Open `config.org` and find the `elfeed-feeds` list inside the elfeed `use-package` block.
2. Parse `$ARGUMENTS`: the first argument is the URL, an optional second argument is a tag.
3. If a tag is provided, add the entry as `("URL" tag)`. Otherwise add it as a plain string `"URL"`.
4. Insert the new entry before the closing `))` of the feeds list, matching the existing indentation.
5. Regenerate `config.el` by running: `emacs --batch --init-directory=. --eval "(progn (require 'org) (org-babel-tangle-file \"config.org\"))"`
6. Report the added feed to the user.

## Rules

- Do NOT modify any other part of config.org.
- Do NOT duplicate a feed that already exists in the list.
- Keep the existing formatting and indentation style.
