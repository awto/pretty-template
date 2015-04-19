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

        package com.example.TestPackage;

        ${imports */vcat $[import ${%$}.*;$]$}

        public class ${name$} ${parent $[extends ${%$}$]$} {
                ${members */?{
                        field : $[${visibility$} ${ty$} ${name/>> $};$] ,
                        method:$[${visibility$} ${result$} ${name$}(${args *$[${ty$} ${name$}$]$}) {
                                  ${body$}
                             }$]
                        }
               $}
        }


Input values may look like:

       test1 :: ClassDef
       test1 = ClassDef {
          imports = ["java.com","java.class"],
          name = "TestClass",
          parent = Just "TestParent",
          members = [
           Field {
             mVisibility = Private,
             mTy = "int",
             mName = "myvar"
           },
           Method {
             mVisibility = Public,
             mResult = "int",
             mName = "getMyVar",
             mArgs = [],
             mBody = "return myvar;"
           },
           Method {
             mVisibility = Public,
             mResult = "void",
             mName = "setMyVar",
             mArgs = [ArgDef "int" "v"],
             mBody = "myvar = v;"
           }
          ]
        }

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
JSON or XML. Here aeson's JSON "Value" type is used as input data. In turn most
of plain Haskell's types may be used via template haskell or generics.

In the templating system the tags are used for selecting sub-template to apply. 
The tag is any string field of an object in parameter. By default it is "tag"
field.  It  simply matches the name of the tag to the name of corresponding 
sub-template. In  template syntax corresponding expression may be defined by c
omma-separated pairs  of tag name and template expression to apply if 
corresponding value is tagged with that name. The name and the template 
expression are separated by colon and whole case expression is enclosed into 
curly braces. In the example by means of case expression the system emits either 
field or methods definitions.

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
