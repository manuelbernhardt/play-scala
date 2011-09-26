package play.templates {

    import java.io.File
    import play.Play
    import play.libs.IO
    import play.libs.Codec
    import play.vfs.VirtualFile
    import scala.annotation.tailrec

    class TemplateCompilationError(source:VirtualFile, message:String, line:Int, column:Int) extends play.exceptions.PlayException("Template compilation error") {

        override def getErrorTitle = getMessage
        override def getErrorDescription = "The template <strong>" + getSourceFile + "</strong> cannot be compiled: <strong>" + message + "</strong>"
        override def getSourceFile = source.relativePath
        override def getLineNumber = line
        override def isSourceAvailable = true

        def getSource = {
            val lines = source.contentAsString.split('\n') :+ ""
            lines.patch(line - 1, Seq(lines(line - 1).patch(column - 1, "↓", 0)), 1)
        }

    }

    class TemplateExecutionError(source:VirtualFile, message:String, line:Int) extends play.exceptions.PlayException("Template execution error") {

        override def getErrorTitle = getMessage
        override def getErrorDescription = "Execution error occured in template <strong>" + getSourceFile + "</strong>: " + message
        override def getSourceFile = source.relativePath
        override def getLineNumber = line
        override def isSourceAvailable = true

        def getSource = source.contentAsString.split('\n') :+ ""

    }

    object Reporter {

        def toHumanException(e:Throwable) = {

            val origin = e.getStackTrace.filter { element =>
                Play.classes.hasClass(element.getClassName)
            }.headOption.flatMap { origin =>
                if(Play.classes.getApplicationClass(origin.getClassName).javaFile.getRealFile.getParentFile == ScalaTemplateCompiler.generatedDirectory) {
                    Some(origin)
                } else {
                    None
                }
            }

            origin.map { origin =>
                val source = Play.classes.getApplicationClass(origin.getClassName).javaFile
                val generated = GeneratedSource(source.getRealFile)
                val originalLine = generated.mapLine(origin.getLineNumber)
                val error = new TemplateExecutionError(VirtualFile.open(generated.source.get), e.getMessage, originalLine)
                error.setStackTrace(e.getStackTrace)

                error
            }.getOrElse(e)

        }

    }

    case class GeneratedSource(file:File) {

        lazy val meta:Map[String,String] = {
            val Meta = """([A-Z]+): (.*)""".r
            val UndefinedMeta = """([A-Z]+):""".r
            Map.empty[String,String] ++ IO.readContentAsString(file).split("-- GENERATED --")(1).trim.split('\n').map { m =>
                m.trim match {
                    case Meta(key, value) => (key -> value)
                    case UndefinedMeta(key) => (key -> "")
                    case _ => ("UNDEFINED", "")
                }
            }
        }

        lazy val matrix:Seq[(Int,Int)] = {
            for(pos <- meta("MATRIX").split('|'); val c = pos.split("->"))
                yield try {
                    Integer.parseInt(c(0)) -> Integer.parseInt(c(1))
                } catch {
                    case _ => (0,0) // Skip if MATRIX meta is corrupted
                }
        }

        lazy val lines:Seq[(Int,Int)] = {
            for(pos <- meta("LINES").split('|'); val c = pos.split("->"))
                yield try {
                    Integer.parseInt(c(0)) -> Integer.parseInt(c(1))
                } catch {
                    case _ => (0,0) // Skip if LINES meta is corrupted
                }
        }

        def needRecompilation = (
            !file.exists ||
             // A generated source already exist but
             source.isDefined && ((source.get.lastModified > file.lastModified) || // the source has been modified
                                  (meta("HASH") != Codec.hexSHA1(VirtualFile.open(source.get).contentAsString))) // or the hash don't match
        )

        def mapPosition(generatedPosition:Int) = {
            matrix.findIndexOf(p => p._1 > generatedPosition) match {
                case 0 => 0
                case i if i > 0 => {
                    val pos = matrix(i-1)
                    pos._2 + (generatedPosition - pos._1)
                }
                case _ => {
                    val pos = matrix.takeRight(1)(0)
                    pos._2 + (generatedPosition - pos._1)
                }
            }
        }

        def mapLine(generatedLine:Int) = {
            lines.findIndexOf(p => p._1 > generatedLine) match {
                case 0 => 0
                case i if i > 0 => {
                    val line = lines(i-1)
                    line._2 + (generatedLine - line._1)
                }
                case _ => {
                    val line = lines.takeRight(1)(0)
                    line._2 + (generatedLine - line._1)
                }
            }
        }

        def toSourcePosition(marker:Int):(Int,Int) = {
            try {
                val targetMarker = mapPosition(marker)
                val line = VirtualFile.open(source.get).contentAsString.substring(0, targetMarker).split('\n').size
                (line,targetMarker)
            } catch {
                case _ => (0,0)
            }
        }

        def source:Option[File] = {
            val s = Play.getVirtualFile(meta("SOURCE"))
            if(s == null || !s.exists) {
                None
            } else {
                Some(s.getRealFile)
            }
        }

        def sync() {
            if (file.exists && !source.isDefined) {
                file.delete()
            }
        }

    }

    case class GeneratedText(file:File) {

      lazy val source = GeneratedSource(new File(file.getAbsolutePath.split("\\.")(0) + ".scala"))

      val separator = "__//__"

      lazy val hash = IO.readContentAsString(file).split(separator)(0)

      def needsRecompilation = {
        !file.exists() || source.source.isDefined && source.source.get.lastModified() > file.lastModified()
      }

      def sync() {
          if (file.exists && !source.source.isDefined) {
              file.delete()
          }
      }

    }

    object ScalaTemplateCompiler {

        import scala.util.parsing.input.Positional
        import scala.util.parsing.input.CharSequenceReader
        import scala.util.parsing.combinator.JavaTokenParsers

        abstract class TemplateTree
        abstract class ScalaExpPart

        case class Params(code:String) extends Positional
        case class Template(name:PosString, params:PosString, imports:Seq[Simple], defs:Seq[Def], sub:Seq[Template], content:Seq[TemplateTree]) extends Positional
        case class PosString(str:String) extends Positional {
            override def toString = str
        }
        case class Def(name:PosString, params:PosString, code:Simple) extends Positional
        case class Plain(text:String) extends TemplateTree  with Positional
        case class Display(exp:ScalaExp) extends TemplateTree  with Positional
        case class ScalaExp(parts:Seq[ScalaExpPart]) extends TemplateTree  with Positional
        case class Simple(code:String) extends ScalaExpPart  with Positional
        case class Block(whitespace: String, args:Option[String], content:Seq[TemplateTree]) extends ScalaExpPart  with Positional
        case class Value(ident:PosString, block:Block) extends Positional

        def compile(source:File) {
            val (templateName,generatedSource) = generatedFile(source)
            val generatedText = generatedTextFile(source)
            val templateType = templateName.takeRight(2).head
            val generator = templateType match {
                case "html" => generateFinalTemplate _
                case "txt" => generateFinalTemplate1 _
                case other => error("unsupported extension :"+other)
            }

            val templateSource = VirtualFile.open(source)


            def parse(generate: Template => List[String]) = {
              templateParser.parser(new CharSequenceReader(templateSource.contentAsString)) match {
                  case templateParser.Success(parsed, rest) if rest.atEnd => {
                    generate(parsed)
                  }
                  case templateParser.Success(_, rest) => {
                      throw new TemplateCompilationError(VirtualFile.open(source), "Not parsed?", rest.pos.line, rest.pos.column)
                  }
                  case templateParser.NoSuccess(message, input) => {
                      throw new TemplateCompilationError(VirtualFile.open(source), message, input.pos.line, input.pos.column)
                  }
              }
            }

            if(Play.mode == Play.Mode.PROD && generatedSource.needRecompilation) {
              val generated = parse { parsed =>

                val generated = generator(templateSource,
                          templateName.dropRight(1).mkString("."),
                          templateName.takeRight(1).mkString,
                          Some(generatedSource.file.getAbsolutePath.split("\\.")(0) + ".plain"),
                          parsed)

                List(generated._1)
              }
              IO.writeContent(generated.head.toString, generatedSource.file)
            } else if(Play.mode == Play.Mode.DEV && generatedText.needsRecompilation) {

              val generated = parse { parsed =>

                val generatedText = generateTextTemplate(
                  templateSource,
                  templateName.dropRight(1).mkString("."),
                  templateName.takeRight(1).mkString,
                  parsed,
                  templateType
                )

                val generatedSourceCode = generator(templateSource,
                          templateName.dropRight(1).mkString("."),
                          templateName.takeRight(1).mkString,
                          Some(generatedSource.file.getAbsolutePath.split("\\.")(0) + ".plain"),
                          parsed)

                val needSourceRecompilation = !generatedSource.file.exists || (generatedSource.meta("SOURCEHASH") != generatedSourceCode._2)

                if(needSourceRecompilation) List(generatedSourceCode._1, generatedText) else List(generatedText)
              }

              if(generated.length == 2) {
                IO.writeContent(generated(0).toString, generatedSource.file)
                IO.writeContent(generated(1).toString, generatedText.file)
              } else {
                IO.writeContent(generated(0).toString, generatedText.file)
              }

            }

        }

        lazy val generatedDirectory = {
            val dir = new File(Play.tmpDir, "generated")
            dir.mkdirs()
            dir
        }

        def generatedFile(template:File) = {
            val templateName = source2TemplateName(template).split('.')
            templateName -> GeneratedSource(new File(generatedDirectory, templateName.mkString("_") + ".scala"))
        }

        def generatedTextFile(template:File) = {
          val templateName = source2TemplateName(template).split('.')
          GeneratedText(new File(generatedDirectory, templateName.mkString("_") + ".plain"))
        }

        @tailrec def source2TemplateName(f:File, suffix:String = ""):String = {
            val Name = """([a-zA-Z0-9_]+)[.]scala[.]([a-z]+)""".r
            (f, f.getName) match {
                case (f, Name(name,ext)) if f.isFile => source2TemplateName(f.getParentFile, ext + "." + name)
                case (_, "views") => "views." + suffix
                case (f, name) => source2TemplateName(f.getParentFile, name + "." + suffix)
            }
        }

        val templateParser = new JavaTokenParsers {

            def as[T](parser:Parser[T], error:String) = {
                Parser(in => parser(in) match {
                    case s:Success[T] => s
                    case Failure(_, next) => Failure("`" + error + "' expected but `" + next.first + "' found", next)
                    case Error(_, next) => Error(error, next)
                })
            }

            def several[T](p: => Parser[T]): Parser[List[T]] = Parser { in =>
                import scala.collection.mutable.ListBuffer
                val elems = new ListBuffer[T]
                def continue(in: Input): ParseResult[List[T]] = {
                    val p0 = p    // avoid repeatedly re-evaluating by-name parser
                    @tailrec def applyp(in0: Input): ParseResult[List[T]] = p0(in0) match {
                        case Success(x, rest) => elems += x ; applyp(rest)
                        case Failure(_, _)    => Success(elems.toList, in0)
                        case err:Error        => err
                    }
                    applyp(in)
                }
                continue(in)
            }

            def at = "@"

            def eof = """\Z""".r

            def identifier = as(ident, "identifier")

            def whiteSpaceNoBreak = """[ \t]+""".r

            def escapedAt = at ~> at

            def any = {
                Parser(in => if(in.atEnd) {
                    Failure("end of file", in)
                } else {
                    Success(in.first, in.rest)
                })
            }

            def plain:Parser[Plain] = {
                positioned(
                    ((escapedAt | (not(at) ~> (not("{" | "}") ~> any)) ) +) ^^ {
                        case charList => Plain(charList.mkString)
                    }
                )
            }
            
            def squareBrackets:Parser[String] = {
                "[" ~ (several((squareBrackets | not("]") ~> any))) ~ commit("]") ^^ {
                    case p1~charList~p2 => p1 + charList.mkString + p2
                }
            }

            def parentheses:Parser[String] = {
                "(" ~ (several((parentheses | not(")") ~> any))) ~ commit(")") ^^ {
                    case p1~charList~p2 => p1 + charList.mkString + p2
                }
            }

            def brackets:Parser[String] = {
                ensureMatchedBrackets( (several((brackets | not("}") ~> any))) ) ^^ {
                    case charList => "{" + charList.mkString + "}"
                }
            }

            def ensureMatchedBrackets[T](p:Parser[T]):Parser[T] = Parser { in =>
                val pWithBrackets = "{" ~> p <~ ("}" | eof ~ err("EOF"))
                pWithBrackets(in) match {
                    case s:Success[T] => s
                    case f:Failure => f
                    case Error("EOF", _) => Error("Unmatched bracket", in)
                    case e:Error => e
                }
            }

            def block:Parser[Block] = {
                positioned(
                    (whiteSpaceNoBreak?) ~ ensureMatchedBrackets( (blockArgs?) ~ several(mixed) ) ^^ {
                        case w~(args~content) => Block(w.getOrElse(""), args, content.flatten)
                    }
                )
            }

            def blockArgs:Parser[String] = """.*=>""".r

            def methodCall:Parser[String] = identifier ~ (squareBrackets?) ~ (parentheses?) ^^ {
                case methodName~types~args => methodName + types.getOrElse("") + args.getOrElse("")
            }

            def expression:Parser[Display] = {
                at ~> commit(positioned(methodCall ^^ {case code => Simple(code)})) ~ several(expressionPart) ^^ {
                    case first~parts => Display(ScalaExp(first :: parts))
                }
            }

            def expressionPart:Parser[ScalaExpPart] = {
                chainedMethods | block | (whiteSpaceNoBreak ~> scalaBlockChained) | elseCall | (parentheses ^^ {case code => Simple(code)})
            }

            def chainedMethods:Parser[Simple] = {
                positioned(
                    "." ~> rep1sep(methodCall, ".") ^^ {
                         case calls => Simple("." + calls.mkString("."))
                     }
                )
            }

            def elseCall:Parser[Simple] = {
                (whiteSpaceNoBreak?) ~> positioned("else" ^^ {case e => Simple(e)}) <~ (whiteSpaceNoBreak?)
            }

            def safeExpression:Parser[Display] = {
                at ~> positioned(parentheses ^^ {case code => Simple(code)}) ^^ {
                    case code => Display(ScalaExp(code :: Nil))
                }
            }

            def matchExpression:Parser[Display] = {
                at ~> positioned(identifier ~ whiteSpaceNoBreak ~ "match" ^^ {case i~w~m => Simple(i+w+m)}) ~ block ^^ {
                    case expr~block => {
                        Display(ScalaExp(List(expr, block)))
                    }
                }
            }

            def forExpression:Parser[Display] = {
                at ~> positioned("for" ~ parentheses ^^ {case f~p => Simple(f+p+" yield ")}) ~ block ^^ {
                    case expr~block => {
                        Display(ScalaExp(List(expr, block)))
                    }
                }
            }

            def caseExpression:Parser[ScalaExp] = {
                (whiteSpace?) ~> positioned("""case (.+)=>""".r ^^ {case c => Simple(c)}) ~ block <~ (whiteSpace?) ^^ {
                    case pattern~block => ScalaExp(List(pattern, block))
                }
            }

            def importExpression:Parser[Simple] = {
                positioned(
                    at ~> """import .*\n""".r ^^ {
                        case stmt => Simple(stmt)
                    }
                )
            }

            def scalaBlock:Parser[Simple] = {
                at ~> positioned(
                    brackets ^^ {case code => Simple(code)}
                )
            }

            def scalaBlockChained:Parser[Block] = {
                scalaBlock ^^ {
                    case code => Block("", None, ScalaExp(code :: Nil) :: Nil)
                }
            }

            def scalaBlockDisplayed:Parser[Display] = {
                scalaBlock ^^ {
                    case code => Display(ScalaExp(code :: Nil))
                }
            }

            def mixed:Parser[Seq[TemplateTree]] = {
                ((scalaBlockDisplayed | caseExpression | matchExpression | forExpression | safeExpression | plain | expression) ^^ {case t => List(t)}) |
                ("{" ~ several(mixed) ~ "}") ^^ {case p1~content~p2 => Plain(p1) +: content.flatten :+ Plain(p2)}
            }

            def template:Parser[Template] = {
                templateDeclaration ~ """[ \t]*=[ \t]*[{]""".r ~ templateContent <~ "}" ^^ {
                    case declaration~assign~content => {
                        Template(declaration._1, declaration._2, content._1, content._2, content._3, content._4)
                    }
                }
            }

            def localDef:Parser[Def] = {
                templateDeclaration ~ """[ \t]*=[ \t]*""".r ~ scalaBlock ^^ {
                    case declaration~w~code => {
                        Def(declaration._1, declaration._2, code)
                    }
                }
            }

            def templateDeclaration:Parser[(PosString,PosString)] = {
                at ~> positioned(identifier ^^ {case s => PosString(s)}) ~ positioned(several(parentheses) ^^ {case s => PosString(s.mkString)}) ^^ {
                    case name~params => name -> params
                }
            }

            def templateContent:Parser[(List[Simple],List[Def],List[Template],List[TemplateTree])] = {
                (several(importExpression | localDef | template | mixed)) ^^ {
                    case elems => {
                        elems.foldLeft((List[Simple](),List[Def](),List[Template](),List[TemplateTree]())) { (s,e) =>
                            e match {
                                case i:Simple => (s._1 :+ i, s._2, s._3, s._4)
                                case d:Def => (s._1, s._2 :+ d, s._3, s._4)
                                case v:Template => (s._1, s._2, s._3 :+ v, s._4)
                                case c:Seq[TemplateTree] => (s._1, s._2, s._3, s._4 ++ c)
                            }
                        }
                    }
                }
            }

            def parser:Parser[Template] = {
                opt(opt(whiteSpaceNoBreak) ~> at ~> positioned((parentheses+) ^^ {case s => PosString(s.mkString)})) ~ templateContent ^^ {
                    case args~content => {
                        Template(PosString(""), args.getOrElse(PosString("()")), content._1, content._2, content._3, content._4)
                    }
                }
            }

            override def skipWhitespace = false

        }

        def visit(elem:Seq[TemplateTree], previous:Seq[Any], textFile: Option[String], position:java.util.concurrent.atomic.AtomicInteger):Seq[Any] = {
            elem match {
                case head :: tail =>
                    val tripleQuote = "\"\"\""
                    def plainText(text:String, p:scala.util.parsing.input.Position): Seq[Any] = {
                      if(Play.mode == Play.Mode.DEV) {
                        Nil :+ Source("_read_text_(" + position + ", \"" +  textFile.get + "\")", p)
                      } else {
                      "format.raw" :+ Source("(", p) :+ tripleQuote :+ text :+ tripleQuote :+ ")"
                      }
                    }
                    visit(tail, head match {
                        case p@Plain(text) => position.incrementAndGet(); (if(previous.isEmpty) Nil else previous :+ ",") :+ plainText(text, p.pos)
                        case Display(exp) => (if(previous.isEmpty) Nil else previous :+ ",") :+ "_display_(List(" :+ visit(Seq(exp), Nil, textFile, position) :+ "))"
                        case ScalaExp(parts) => previous :+ parts.map {
                            case s@Simple(code) => Source(code, s.pos)
                            case b@Block(whitespace,args,content) => Nil :+ Source(whitespace + "{" + args.getOrElse(""), b.pos) :+ "_display_(List(" :+ visit(content, Nil, textFile, position) :+ "))}"
                        }
                    }, textFile, position)
                case Nil => previous
            }
        }

       def _display_(o:Any, format:String):Any = {
            val f = format match {
              case "html" => HtmlFormat
              case "txt" => PlainFormat
              case other => error("unexpected extension: " + other)
            }
            o match {
                case escapedHtml:Html => escapedHtml
                case escapedPlain:Plain => escapedPlain
                case () => f.raw("")
                case None => f.raw("")
                case Some(v) => _display_(v, format)
                case escapeds:Iterable[_] => {
                  val formatted = escapeds.map(e => _display_(e, format))
                  if(!formatted.isEmpty) {
                    formatted.reduceLeft((a, b) => {
                      if(a.toString.isEmpty && b.toString.isEmpty) ""
                      else a.toString + "__//__" + b.toString
                    })
                  } else {
                    f.raw("")
                  }
                }
                case string:String => string
                case _ => f.raw("")
            }
        }

        def visitText(elem:Seq[TemplateTree], previous:Seq[Any], format:String):Seq[Any] = {
          elem match {
            case head :: tail =>
              visitText(tail, head match {
                case p@Plain(text) => (if(previous.isEmpty) Nil else previous) :+ text
                case Display(exp) => (if(previous.isEmpty) Nil else previous) :+ _display_(visitText(Seq(exp), Nil, format), format)
                case ScalaExp(parts) => previous :+ parts.map {
                    case s@Simple(code) => Nil
                    case b@Block(whitespace,args,content) => Nil :+ _display_(visitText(content, Nil, format), format)
                }
                case _ => Nil
              }, format)
            case Nil => previous
          }
        }

        def templateCode(template:Template, textFile:Option[String] = None):Seq[Any] = {

            val defs = (template.sub ++ template.defs).map { i =>
                i match {
                    case t:Template if t.name == "" => templateCode(t, textFile)
                    case t:Template => {
                        Nil :+ """def """ :+ Source(t.name.str, t.name.pos) :+ Source(t.params.str, t.params.pos) :+ " = {" :+ templateCode(t, textFile) :+ "};"
                    }
                    case Def(name, params, block) => {
                        Nil :+ """def """ :+ Source(name.str, name.pos) :+ Source(params.str, params.pos) :+ " = {" :+ block.code :+ "};"
                    }
                }
            }

            val imports = template.imports.map(_.code).mkString("\n")

            Nil :+ imports :+ "\n" :+ defs :+ "\n" :+ "List(" :+ visit(template.content, Nil, textFile, new java.util.concurrent.atomic.AtomicInteger(0)) :+ ")"
        }

      def generateFinalTemplate1(template: VirtualFile, packageName: String, name: String, textFile: Option[String], root:Template) = {

            val textFile = if(Play.mode == Play.Mode.DEV) Some(template.getRealFile.getAbsolutePath.split("\\.")(0) + ".plain") else None

            val generated = {
                Nil :+ """
                    package """ :+ packageName :+ """

                    import play.templates._
                    import play.templates.TemplateMagic._

                    object """ :+ name :+ """ extends BaseScalaTemplate[Plain,Format[Plain]](PlainFormat) {

                        def apply""" :+ Source(root.params.str, root.params.pos) :+ """:Plain = {
                            try {
                                _display_ {""" :+ templateCode(root,textFile) :+ """}
                            } catch {
                                case e:TemplateExecutionError => throw e
                                case e => throw Reporter.toHumanException(e)
                            }
                        }

                    }

                """
            }

            Source.finalSource(template, generated)
        }

        def generateFinalTemplate(template: VirtualFile, packageName: String, name: String, textFile: Option[String], root:Template) = {

            val generated = {
                Nil :+ """
                    package """ :+ packageName :+ """

                    import play.templates._
                    import play.templates.TemplateMagic._
                    import views.html._

                    object """ :+ name :+ """ extends BaseScalaTemplate[Html,Format[Html]](HtmlFormat) {

                        def apply""" :+ Source(root.params.str, root.params.pos) :+ """:Html = {
                            try {
                                _display_ {""" :+ templateCode(root,textFile) :+ """}
                            } catch {
                                case e:TemplateExecutionError => throw e
                                case e => throw Reporter.toHumanException(e)
                            }
                        }

                    }

                """
            }

            Source.finalSource(template, generated)
        }

        def generateTextTemplate(template: VirtualFile, packageName: String, name: String, root:Template, format:String) = {
            val parsed = visitText(root.content, Nil, format)
            val hash = Codec.hexSHA1(parsed.mkString)
            val sep = "__//__"
            val serialized = if(parsed.isEmpty) {
              ""
            } else if(parsed.length == 1) {
              parsed.head
            } else {
              parsed.reduceLeft((r, c) => {
                if(r.toString.endsWith(sep) && !c.toString.startsWith(sep)) r.toString + c.toString
                else if(!r.toString.endsWith(sep) && c.toString.startsWith(sep)) r.toString + c.toString
                else if(!r.toString.endsWith(sep) && !c.toString.startsWith(sep)) r.toString + sep + c.toString
                else ""
              })
            }

            hash + sep + serialized.toString.replaceAll("__//____//__", "__//__")
        }

    }

    /* ------ */

    import scala.util.parsing.input.{Position, OffsetPosition, NoPosition}

    case class Source(code:String, pos:Position = NoPosition)

    object Source {

        import scala.collection.mutable.ListBuffer

        def finalSource(template:VirtualFile, generatedTokens:Seq[Any]) = {
            val scalaCode = new StringBuilder
            val positions = ListBuffer.empty[(Int,Int)]
            val lines = ListBuffer.empty[(Int,Int)]
            serialize(generatedTokens, scalaCode, positions, lines)
            val scalaCodeHash = Codec.hexSHA1(scalaCode.toString)
            val code = scalaCode + """
                /*
                    -- GENERATED --
                    DATE: """ + new java.util.Date + """
                    SOURCE: """ + template.relativePath.replaceFirst("\\{[^\\}]*\\}", "") + """
                    HASH: """ + Codec.hexSHA1(template.contentAsString) + """
                    SOURCEHASH: """ + scalaCodeHash + """
                    MATRIX: """ + positions.map { pos =>
                        pos._1 + "->" + pos._2
                    }.mkString("|") + """
                    LINES: """ + lines.map { line =>
                        line._1 + "->" + line._2
                    }.mkString("|") + """
                    -- GENERATED --
                */
            """

            (code.toString, scalaCodeHash)
        }

        private def serialize(parts:Seq[Any], source:StringBuilder, positions:ListBuffer[(Int,Int)], lines:ListBuffer[(Int,Int)]) {
            parts.foreach {
                case s:String => source.append(s)
                case Source(code, pos@OffsetPosition(_, offset)) => {
                    source.append("/*" + pos + "*/")
                    positions += (source.length -> offset)
                    lines += (source.toString.split('\n').size -> pos.line)
                    source.append(code)
                }
                case Source(code, NoPosition) => source.append(code)
                case s:Seq[any] => serialize(s, source, positions, lines)
                case _ =>
            }
        }

    }

    /* ------ */

    trait Appendable[T] {
        def +(other:T):T
    }

    trait Format[T<:Appendable[T]] {
        def raw(text:String):T
        def escape(text:String):T
    }

    case class Html(text:String) extends Appendable[Html] {
        val buffer = new StringBuilder(text)

        def +(other:Html) = {
            buffer.append(other.buffer)
            this
        }
        override def toString = buffer.toString
    }

    object Html {

        def empty = Html("")

    }

    object HtmlFormat extends Format[Html] {
        def raw(text:String) = Html(text)
        def escape(text:String) = Html(play.utils.HTML.htmlEscape(text))
    }


    case class Plain(text:String) extends Appendable[Plain] {

        var buffer = new StringBuilder(text)

        def +(other:Plain) = {
            buffer.append(other.buffer)
            this
        }
      override def toString = buffer.toString
    }

    object PlainFormat extends Format[Plain] {
        def raw(text:String) = Plain(text)
        def escape(text:String) = Plain(text)
    }


    

    case class BaseScalaTemplate[T<:Appendable[T],F<:Format[T]](format: F) {

        def _read_text_(pos:Int,f:String):T = {
          val array = play.libs.IO.readContentAsString(new File(f)).split("__//__")
          format.raw(array(pos))
        }

        def _display_(o:Any):T = {
            o match {
                case escaped:T => escaped
                case () => format.raw("")
                case None => format.raw("")
                case Some(v) => _display_(v)
                case escapeds:Iterable[_] => escapeds.foldLeft(format.raw(""))(_ + _display_(_))
                case string:String => format.escape(string)
                case v if v != null => _display_(v.toString)
                case _ => format.raw("")
            }
        }

    }

    /* ------ */

    object TemplateMagic {

        // --- IF

        implicit def iterableToBoolean(x:Iterable[_]) = x != null && !x.isEmpty
        implicit def optionToBoolean(x:Option[_]) = x != null && x.isDefined
        implicit def stringToBoolean(x:String) = x != null && !x.isEmpty

        // --- JAVA

        implicit def javaCollectionToScala(x:java.lang.Iterable[_]) = {
            import scala.collection.JavaConverters._
            x.asScala
        }
        
        // --- DEFAULT

        case class Default(default:Any) {
            def ?:(x:Any) = x match {
                case "" => default
                case Nil => default
                case false => default
                case 0 => default
                case None => default
                case _ => x
            }
        }

        implicit def anyToDefault(x:Any) = Default(x)

        // --- DATE

        class RichDate(date:java.util.Date) {

            def format(pattern:String) = {
                new java.text.SimpleDateFormat(pattern).format(date)
            }

        }

        implicit def richDate(date:java.util.Date) = new RichDate(date)

        // --- STRING

        class RichString(string:String) {

            def when(predicate: => Boolean) = {
                predicate match {
                    case true => string
                    case false => ""
                }
            }

        }

        implicit def richString(string:String) = new RichString(string)

        // --- ROUTER

        def action(action: => Any) = {
            new play.mvc.results.ScalaAction(action).actionDefinition.url
        }

        def asset(path:String) = play.mvc.Router.reverse(play.Play.getVirtualFile(path))

    }

}

package views {

    package object html {

        import play.templates.Html

        def form(action: => Any)(body: => Html) = Html {
            var actionDef = new play.mvc.results.ScalaAction(action).actionDefinition
            """<form action="""" + actionDef.url + """" method="""" + (if(actionDef.star) "POST" else actionDef.method) + """">""" + body + """</form>"""
        }

        def a(action: => Any)(body: => Html) = Html {
            var actionDef = new play.mvc.results.ScalaAction(action).actionDefinition
            if(actionDef.method == "GET") {
                """<a href="""" + actionDef.url + """">""" + body + """</a>"""
            } else {
                val uuid = play.libs.Codec.UUID
                """<a href="javascript:document.getElementById('""" + uuid + """').submit()">""" + body + """</a><form id="""" + uuid + """" action="""" + actionDef.url + """" method="""" + actionDef.method + """"></form>"""
            }
        }

    }

    package object txt {}

}
