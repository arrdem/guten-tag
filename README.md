# Guten tag

> Good tags or good day, whichever!

[![Clojars Project](http://clojars.org/me.arrdem/guten-tag/latest-version.svg)](http://clojars.org/me.arrdem/guten-tag)

Types are really cool.
They allow you to express limitations on the code you've written and reason about what could possibly work.
Most of all, they can help you reason about what you _want_ to work.
In Clojure we have some types, but more often than not we have sum types.
As [jneen said at Clojure/Conj '14](https://www.youtube.com/watch?v=ZQkIWWTygio) the common pattern in most Clojure code is to use a sum type (a map) to encode what would in other types be a record especially since Clojure's records as a gen-class have limitations with regards to reloading.

That is to say, in C if I wanted to express a person with some data I'd wind up with the following:

```C
typedef struct {
   char *full_name,
        *email_addr,
		*employer;
} person;
```

Which is awful for all the usual reasons that go with manual memory management, but critically the structure encodes no information about exactly what it is.
If you throw me a `void*`, I have no way to figure out exactly what data you gave me.
The real fun of C is that I can just type cast it away and do whatever the heck I want with an arbitrary integer but we're talking Clojure not C here.

In comparison, the Clojure idiom would be to write the following structure:

```Clojure
{:type       :person
 :full-name  "",
 :email-addr "",
 :employer   ""}
```

Which is great for many reasons, but as jneen argued in her talk, it has issues with validation, nils can slip in, and the type key is advisory.
To paraphrase a 40 minute talk, the above map represents the sum of a type name and a bunch of data.
What we really want is the product of a type name and a bunch of data so that the type clearly identifies the data and can't get lost.
The pattern that jneen points to is to use one of the following:

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

I reject the latter as being inflexible and as having bitten me in the past.
Which gives me a keyword tagged map.
This is great for the purpose of dispatching on the type and making sure that you don't loose the type.
However the thing that makes the map pattern awesome is not just that we can do map destructuring on it, but that we can do updating on it easily.
After messing with the vector tagging a map pattern for a while, I came to the conclusion that it was really awkward to be writing `(update-in val [1 my-key] my-updater)` all the time to preserve the key.
When I wanted to get a value out of the tagged map, I had to do something like `(let [[tag {:keys [foo bar]} :as qux] ...] ...)`.
It was just awkward and it interfered with Clojure's idioms especially without real language level pattern matching ala Haskell (sorry dnolen core.match needs a lot of syntactic work).

The solution I came up with was to hack out a new class which behaves just like the above tag prefixed map when you `seq` it and thus for core.match, but which keeps the tag out of the way when you want to get or update keys and thus behaves kinda like a struct.
This is exactly the purpose of the `guten_tag.core.ATaggedVal` class.
It provides the following interfaces

 - `clojure.lang.ILookup`
 - `clojure.lang.Associative`
 - `clojure.lang.ISeq`
 - `clojure.lang.Indexed`

so it'll behave like the seq `(<tag> <tag wrapped map>)`.
`clojure.core/first` and `clojure.core/second` will work great and give you respectively the tagged value as you would expect.
It's associative so `clojure.core/get` and the other associative get operators will be able to pull keys out the tag wrapped map.
It also means that `clojure.core/assoc` and `clojure.core/update` will "just work" as if you were using the bare map.

The interface `guten_tag.core.ITaggedVal` exists to provide the `-tag` and `-val` methods, wrapped in true ClojureScript style in the `tag` and `val` fns.
The predicate `tagged?` is also provided.

Reader/printer notation for tagged values is provided:

```Clojure
user> (require '[guten-tag.core :as t])
nil
user> (t/deftag foo [bar])
nil
user> (->foo 3)
#guten/tag [:user/foo {:bar 3}]
```

The `deftag` macro is a little special in that as of this writing it does three things:

 - Generate a `->T` constructor
 - Generate a `T?` predicate
 - Generate a `T` var describing the `T` tagged type as defined.

The `deftag` macro is of the syntax

```Clojure
(deftag name-sym
  members-vec
  docstring?
  attrs-map?)
```

Note the similarities to `clojure.core/defn`.
Like `defn`, `deftag` supports `:pre` and `:post` in the attrs map.
Preconditions and postconditions in the attrs map will be applied both to the generated predicate and to the generated constructor.
The `:pre` capabilities of `deftag` can be used to emulate [smart constructors](https://wiki.haskell.org/Smart_constructors).
This pattern worked very well for me [in lib-grimoire](https://github.com/clojure-grimoire/lib-grimoire/blob/master/src/grimoire/things.clj#L23-L90) and I highly recommend it as a mechanism for enforcing ongoing data sanity checks.


## License

Copyright © 2015 Reid 'arrdem' McKenzie

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
