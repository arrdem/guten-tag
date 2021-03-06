# Guten tag

> Good tags for a good day!

[![Clojars Project](http://clojars.org/me.arrdem/guten-tag/latest-version.svg)](http://clojars.org/me.arrdem/guten-tag)

## Demo

```Clojure
(require '[guten-tag.core :as t])
;; => nil

(t/deftag foo
  [bar])
;; => nil

(->foo 3)
;; => #g/t [:user/foo {:bar 3}]

(def base-foo *1)
;; => #'user/base-foo

(t/tag base-foo)
;; => :user/foo

(t/val base-foo)
;; => {:bar 3}

(foo? base-foo)
;; => true

(assoc base-foo :foo 'bar)
;; => #g/t [:user/foo {:bar 3, :foo bar}]

(seq *1)
;; => (:user/foo {:bar 3 :foo bar})
```

support for [core.match](https://github.com/clojure/core.match) is provided as a sequence type:

```Clojure
(require '[clojure.core.match
           :refer [match]])
;; => nil

(require '[guten-tag.core :as t])
;; => nil

(t/deftag person
  [name email phone]) ;; people are chill
;; => nil

(t/deftag dog
  [name owner breed]) ;; dogs are awesome
;; => nil

(defn f [t]
  (match [t]
    [([::person {:name name}] :seq)]
    ,,name    ;; who?
    
    [([::dog {:breed "boxer"}] :seq)]
    ,,"<3"    ;; I DON'T CARE WHO YAAS
    
    [([::dog {:owner owner}] :seq)]
    ,,owner)) ;; who?
;; => #'user/f

(f (->person "reid" "me@arrdem.com" "XXX-YYY-ZZZZ"))
;; => "reid"

(f (->dog "papu" "callen" "mix"))
;; => "callen"

(f (->dog "tina" "reid" "boxer"))
;; => "<3"
```

## Motivation

Types are really cool.  They allow you to express limitations on the code you've written and reason
about what could possibly work.  Most of all, they can help you reason about what you _want_ to
work.  In Clojure we have some types, but more often than not we have sum types.  As
[jneen said at Clojure/Conj '14](https://www.youtube.com/watch?v=ZQkIWWTygio) the common pattern in
most Clojure code is to use a sum type (a map) to encode what would in other types be a record
especially since Clojure's records as a gen-class have limitations with regards to reloading.

That is to say, in C if I wanted to express a person with some data I'd wind up with the following:

```C
typedef struct {
   char *full_name,
        *email_addr,
		*employer;
} person;
```

Which is awful for all the usual reasons that go with manual memory management, but critically the
structure encodes no information about exactly what it is and isn't open to extension.  It isn't
actually associative at all.

In comparison, the Clojure idiom would be to write the following structure:

```Clojure
{:type       :person
 :full-name  "",
 :email-addr "",
 :employer   ""}
```

Which is great for many reasons being associative, extensible and immutable, but as jneen argued in
her talk, it has issues with validation, nils can slip in, and the type key is advisory.  To
paraphrase a 40 minute talk, the above map represents the sum of a type name and a bunch of data.
What we really want is the product of a type name and a bunch of data so that the type clearly
identifies the data and can't get lost.  The pattern that jneen points to is to use one of the
following:

```Clojure
[:person
 {:full-name  "",
  :email-addr "",
  :employer   ""}]
```

or

```Clojure
[:person "" "" ""]
```

The latter seems to complect structural position with type or at least value in a very inflexible
manner. In refactoring such structures, addition is only safe if appending is strictly observed and
deletion is not generally safe. Preserving value naming is more resilient to such problems, which
gives me a strong preference for the keyword tagged map. This is great for the purpose of
dispatching on the type and making sure that you don't loose the type.  However the thing that makes
the map pattern awesome is not just that we can do map destructuring on it, but that we can do
updating on it easily.  After messing with the vector tagging a map pattern for a while, I came to
the conclusion that it was really awkward to be writing `(update-in val [1 my-key] my-updater)` all
the time to preserve the key.  When I wanted to get a value out of the tagged map, I had to do
something like `(let [[tag {:keys [foo bar]} :as qux] ...] ...)`.  It was just awkward and it
interfered with Clojure's idioms especially without real language level pattern matching ala Haskell
(sorry dnolen core.match needs a lot of syntactic work).

The solution I came up with was to hack out a new class which behaves just like the above tag
prefixed map when you `seq` it and thus for core.match, but which keeps the tag out of the way when
you want to get or update keys and thus behaves kinda like a struct.  This is exactly the purpose of
the `guten_tag.core.ATaggedVal` class.  It provides the following interfaces

 - `clojure.lang.ILookup`
 - `clojure.lang.Associative`
 - `clojure.lang.Sequential`
 - `clojure.lang.ISeq`
 - `clojure.lang.Indexed`

so it'll behave like the seq `(<tag> <tag wrapped map>)`.  `clojure.core/first` and
`clojure.core/second` will work great and give you respectively the tagged value as you would
expect.  It's associative so `clojure.core/get` and the other associative get operators will be able
to pull keys out the tag wrapped map.  It also means that `clojure.core/assoc` and
`clojure.core/update` will "just work" as if you were using the bare map.

The interface `guten_tag.core.ITaggedVal` exists to provide the `-tag` and `-val` methods, wrapped
in true ClojureScript style in the `tag` and `val` fns.  The predicate `tagged?` is also provided.

Reader/printer notation for tagged values is provided.

The `deftag` macro is a little special in that as of this writing it does two things:

 - Generate a `->T` constructor
 - Generate a `T?` predicate

The `deftag` macro is of the syntax

```Clojure
(deftag name-sym
  members-vec
  docstring?
  attrs-map?)
```

Note the similarities to `clojure.core/defn`.  Like `defn`, `deftag` supports `:pre` in the attrs
map.  Preconditions in the attrs map will be applied both to the generated predicate and to the
generated constructor.  The `:pre` capabilities of `deftag` can be used to emulate
[smart constructors](https://wiki.haskell.org/Smart_constructors).  This pattern worked very well
for me
[in lib-grimoire](https://github.com/clojure-grimoire/lib-grimoire/blob/master/src/grimoire/things.clj#L23-L90)
and I highly recommend it as a mechanism for enforcing ongoing data sanity checks.  Note that
`:post` is not supported because constructos are an identity operation and you may express any
legitimate postcondition with a precondition.

## License

Copyright © 2015 Reid 'arrdem' McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later
version.
