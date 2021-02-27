---
title: "Lua filter counter"
output:
  html_document:
    pandoc_args: 
    - --lua-filter
    - "counter.lua"
---


**First via inline code** `@@Ref-1`

**Advance to second via the text:** @vt-refA

**Call the value first using inline:** `@@Ref-1`

**Some other code block, no count here:** `something else`

```
more code who knows
```

**third via inline code** `@@Ref-2`

**First ref, inline again** `@@Ref-1`

**second entry, again in the text:** @vt-refA

**Call third again via inline code:** `@@Ref-2`

**use the first tag (inline code) but with text syntax:** @vt-Ref-1

**use the second tax (text) but inline syntax** `@@refA`


