pretty-template
===============

This is a prototype of yet another string tempting library. The key distinction 
from the other libraries is pretty printing options embedded into template's 
syntax. At the present version the options are based on Wadler/Leijen
pretty-printing combinators library. Other systems typically doesn't consider 
layout at all. So either users have to put extra white-spaces destroying 
template's layout or ignore output layout issues at all. It may be considered 
not significant if resulting text is not supposed to be read by human (i.e. 
HTML or LaTeX) or there is some cheap alternatives for beautifying output 
format. However there is quite large space for template's applications where text 
layout does matter. A typical one is generation various kind of text reports, 
likely based on localized strings. In case of code generation well layout output 
helps debugging templates. In some programming languages text layout changes 
semantics.
 
For example let's take a Java class generator task. In current syntax it looks 
like:

        ${imports */ $[import ${%$}$]$}
        
        public class ${name$} ${parent $[extends ${%$}$]$} {
               ${members */{
                         field : $[${visibility$} ${type$} ${name/>> $};$] ,
                         method:$[${visibility$} ${type$} 
                              ${name$}(${args *$[${type$} ${name$}$]$}) {
                                    ${body$}
                              }$]
                         }
              $}       
        }

Input values may look like:

    test1 :: Val
    test1 = obj [
         "imports"  =: Arr [
                        c "java.com", c "java.class"
                       ],
         "name"    =: c "TestClasS",
         "parent"  =: c "ParrenatS",
         "members" =: Arr [
                       "field" :@ obj [
                                    "visibility"  =: c "private",
                                    "type"        =: c "int",
                                    "name"        =: c "myvara\n- 2"
                                   ],
                       "method" :@ obj [
                                     "visibility" =: c "public",
                                     "type"       =: c "int",
                                     "name"       =: c "getMyvara",
                                     "args"       =: Arr [],
                                     -- TODO: body as Doc
                                     "body"       =: c "return myvara;" 
                                    ],
                       "method" :@ obj [
                                     "visibility" =: c "public",
                                     "type"       =: c "void",
                                     "name"       =: c "setMyvara",
                                     "args"       =: Arr [
                                                      obj [
                                                       "type" =: c "int", 
                                                       "name" =: c "v"
                                                      ]
                                                     ],
                                     "body"       =: c "myvara = v;"
                                    ]
                      ]
        ]

Templates have two distinguished levels, namely template levels where template 
expressions are specified and verbatim text level which is simply copied into 
output. In code generation context the levels could be called meta-level and 
object-level.

Here "${...$}" parens switches from text-level to template-level and "$[...$]" back 
from template-level to text-level.

At the moment the library simply converts the template syntax into Wadler/Leijen 
pretty-printing combinators application, though in some future version it is 
worth considering implementing some printing algorithm which is more specific 
for string's template applications. The library also has implements simple 
heuristics for setting indent levels of each line.

Template expressions for substitution of input data starts on name of field 
in the data after that a few other template expressions follow to handle that 
field. If the field value is an atomic value it is just output to result. If 
it is another record a sub template is applied to the record. The sub-template 
is specified just after the field name. The scope of variable in that sub-template 
is within fields of the matched record. Optionally it is possible to set a template 
to apply in case if variable is not defined. It is next template expression which 
starts with dot to be applied in this case.

To get a variable from parent record by means of "^" prefix. Current record can 
be referred by means of % expression. 

For list of values the variable name must be followed by "*" after which one 
required template expression and two optional may be specified. The required 
expression is applied to each value in the list. Optional template expression, 
starting on "," defines punctuation for the values and last argument started 
on "." defines template for empty lists or not defined fields. 

The way the templates are formatted can be controlled by means of options. 
There are two groups of options. One may be applied only to list's template 
expressions the other to over templates. List options are specified after the 
asterisk separated by "/". Options are just identifiers names which correspond 
to concatenation operators from the underlying pretty-printing library. By default 
it is "hcat". If no name is specified after "/" it is considered to be "vsep". 
Other supported values are "hsep", "vsep", "feelsep", "lsep", "hcat", "vcat", 
"fillcat", "cat".

Options which can be applied to any template expression are "group", "hang", 
"nest", "align". And there are a convenient aliases: ">>" for hang, ">" for nest", 
"|" for align, and "-" for not applying anything at all. By default it is always 
align. Expression's options are defined in end of expression and separated by "/". 
It has the lowest syntax priority so it binds to outer expression. Use parentheses 
to group template expressions and bind the options closely.

Rather typically an input of a template may be any structured data format like 
JSON or XML. The prototype implementation doesn't try to read input data, it should 
be provided as Haskell value with Val type. It is quite straightforward reflection 
of untyped encoding of structured data.

The only but significant difference is tagged values (i.e. :@ constructor). It is 
the way to define heterogeneous data structures similar to polymorphic reference 
in OOP language like C++/Java. The feature is not standardized in JSON but it is 
possible to choose some JSON object's field to specify the type. During serialization 
of Java object into XML typically type attribute from XML Schema namespace is used 
for the same purpose. However it may be simulated by some other ways, like element's 
name, or derived from other values.

In the tempting system the tags are used for selecting sub-template to apply. It 
simply matches the name of the tag to the name of corresponding sub-template. In 
template syntax corresponding expression may be defined by comma-separated pairs 
of tag name and template expression to apply if corresponding value is tagged with 
that name. The name and the template expression are separated by colon and whole case 
expression is enclosed into curly braces. In the example by means of case expression 
the system emits either field or methods definitions.

The tag based selection is replacement of if-then-else expressions in template 
language. The system doesn't allow arbitrary control operators to encourage 
separation of templates from business logic. It is one of the main design 
options considered in all tempting systems. Some of them allow arbitrary 
expressions with functions and typical control operators. Others only allow 
filling in expression holes without any facilities to specialize sub-template 
according to input values. This solution is somewhere in the middle. It provides 
quite enough control over template selection and, I believe, it doesn't break 
the wanted separation.

TODO:

* 2D pretty printing
* Staging
* Quasiquoter for embedding into Haskell code
* Template's compilation 
* Production quality
