# uf

> As I was walking by the Great Bear in the northern sky  
> I found the seashell missing from the shore below
> 
> --- Colleen, Ursa Major Find

*uf* is a toy implementation of Forth programming language. It's goal is to understand Forth.

## Installation

Install *uf* with [roswell](https://github.com/roswell/roswell/):

```
$ ros install t-sin/uf
```

## Usage

In REPL, you can use one-shot VM:

```
;; calculating Fibonacci number of 10
CL-USER> (with-input-from-string
             (in "
: <= over over < rot swap = or ;
: fib dup 0 swap <= if drop 0 else dup 1 = if drop 1 else
  dup 1 swap - fib swap 2 swap - fib + then then ;
10 fib")
           (let ((vm (uf:init-vm (uf:parse in))))
             (uf:execute vm)
             vm))
#<VM: (55)>
```

If you want to run *uf* with REPL, roswell script satisfies you:

```
$ ./roswell/ufi.ros
```

If you want to see data stack each input, specify `--debug` option.

## Author

- TANAKA Shinichi (<shinichi.tanaka45@gmail.com>)

## Copyright

Copyright (c) 2018 Shinichi TANAKA (shinichi.tanaka45@gmail.com)

## License

Rosa is licensed under the Lisp GNU Lesser General Public License.
