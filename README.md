# Hemplate
Hemplate is a small, unfinished templating language I developed when making a website with the Spock framework.

## Language
Print variable:
```
{- varName -}
```

Include another template:
```
{- template ../front/header.html -}
```

Iterate through a list of objects:
```
{- iteration rss -}
  <div class="feed" data-href="{- rss.link -}">
    <a href='/feed/delete/{- rss.id -}'>Remove from your feeds</a>
  </div>
{- end -}
``` 

## Usage

So far Hemplate is meant to be used as a Haskell library.

It exports two functions:

```haskell
render :: FilePath -> Document -> IO Text
```
`render` is self explanatory: it is given a file name and a key/value store containing variables, and outputs `Text`.

```haskell
renderHemplate :: FilePath -> FilePath -> Document -> IO Text
```
`renderHemplate` is for rendering one template within another. The position of the inner template is marked by having the `{- yield -}` variable in the outer template. A better name would be `renderWithLayout`, and this will be fixed if I pick up the language again.
