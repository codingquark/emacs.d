---
name: add-feed
description: Add an RSS/Atom feed URL to the elfeed subscriptions in elfeed-feeds.el
user-invocable: true
allowed-tools: Read, Edit, Bash, Grep
argument-hint: "<feed-url> [tag]"
---

# Add Elfeed Feed

Add the feed URL `$ARGUMENTS` to the elfeed subscriptions.

## Steps

1. Open `elfeed-feeds.el` and find the `elfeed-feeds` list.
2. Parse `$ARGUMENTS`: the first argument is the URL, an optional second argument is a tag.
3. If a tag is provided, add the entry as `("URL" tag)`. Otherwise add it as a plain string `"URL"`.
4. Insert the new entry before the closing `))` of the list, matching the existing indentation.
5. Report the added feed to the user.

## Rules

- Only edit `elfeed-feeds.el`. Do NOT touch `config.org` or `config.el`.
- Do NOT duplicate a feed that already exists in the list.
- Keep the existing formatting and indentation style.
