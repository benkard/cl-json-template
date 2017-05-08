# JSON Template for Common Lisp

## Summary

An implementation of [JSON Template][] in Common Lisp.

## Implementation Features

 * No dependencies
 * Portable Common Lisp (tested
   on [SBCL][], [Clozure CL][], [ECL][], [XCL][], [ABCL][])
 * HTML and URI escaping through the use of formatters
 * Apache license

## Missing Things

 * Literals (like `{.space}` and `{.meta-left}`/`{.meta-right}`)
 * Multiple-argument formatters
 * Options (like changing the meta character or the default formatter)
 * Some kind of compilation for efficiency

## Examples

```lisp
JSON-TEMPLATE> (defparameter *tmpl* (parse-template-string "
<h1>{title|html}</h1>
{.section people}
<ul>
{.repeated section @}
  <li>{name} ({age} years)</li>
{.end}
</ul>
{.or}
<p>No one's registered.</p>
{.end}"))
*TMPL*
```

```lisp
JSON-TEMPLATE> (expand-template *tmpl*
                                '(:title "<Registered People>"
                                  :people ((:name "Nathalie" :age 24)
                                           (:name "Heinrich" :age 28)
                                           (:name "Hans"     :age 25))))
"
<h1>&#60;Registered People&#62;</h1>
<ul>
  <li>Nathalie (24 years)</li>
  <li>Heinrich (28 years)</li>
  <li>Hans (25 years)</li>
</ul>
"
```

```lisp
JSON-TEMPLATE> (expand-template *tmpl*
                                '(:title "<Registered People>"
                                  :people ()))
"
<h1>&#60;Registered People&#62;</h1>
<p>No one's registered.</p>
"
```


[JSON Template]: http://jsont.squarespace.com
[SBCL]:          http://www.sbcl.org/
[Clozure CL]:    http://ccl.clozure.com/
[ECL]:           http://ecls.sf.net/
[XCL]:           https://github.com/gnooth/xcl
[ABCL]:          http://common-lisp.net/project/armedbear/
