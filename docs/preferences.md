# Preferences

Like normal Clojure multimethods, Methodical multimethods support specifying preferences for one dispatch value over
another so it knows which method to use in situation where neither of two dispatch values is considered more specific
than another (e.g., neither one `isa?` the other).

## Adding Preferences

[[methodical.core/prefer-method]] works just like `clojure.core/prefer-method`, with one important difference: it is
non-destructive. [[methodical.core/prefer-method]] will return a copy of the original multimethod with an updated
preferences table.

To destructively add a preference to a methodical multimethod, use [[methodical.core/prefer-method!]] and the var
you'd like to update:

```clj
;;; add a preference of :x over y
(m/prefer-method! #'my-multimethod :x :y)
```

You can also non-destructively replace the entire preferences table with a new one using the low-level method
[[methodical.core/with-prefers]], but you should use caution in doing so. [[methodical.core/prefer-method]] will check
to make sure any preferences you add are valid (e.g., no conflicts between preferences), but
[[methodical.core/with-prefers]] will not.

[[methodical.core/with-prefers!]] is a destructive version of [[methodical.core/with-prefers]] for use on multimethod
vars.

## Inspecting Preferences

[[methodical.core/prefers]] works just like `clojure.core/prefers` and can be used to get the preferences map for a
multimethod.

## Removing Preferences

Unlike normal Clojure multimethods, Methodical also supports removing preferences. [[methodical.core/unprefer-method]]
undoes a preference added by [[methodical.core/prefer-method]]. Like [[methodical.core/prefer-method]], this is a
non-destructive function that returns a copy of the original multimethod with an updated preferences map; you can use
[[methodical.core/unprefer-method!]] to destructively update a multimethod var.

[[methodical.core/remove-all-preferences]] non-destructively removes all preferences for a multimethod by replacing
its preferences map with an empty one. [[methodical.core/remove-all-preferences!]] is a destructive version for doing
the same operation on a multimethod var.
