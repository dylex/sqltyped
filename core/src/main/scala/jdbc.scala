package sqltyped

import java.sql._

private[sqltyped] sealed abstract class Type(val jdbcType: Int) {
  def name: String
  def scalaType: String
}
private[sqltyped] object Type {
  import java.sql.Types._
  /* a mapping from java.sql.Types.* values to their getFoo/setFoo names */
  final val names = Map(
      ARRAY         -> "Array"  
    , BIGINT	    -> "Long"   
    , BINARY	    -> "Bytes"  
    , BIT	    -> "Boolean"
    , BLOB	    -> "Blob"   
    , BOOLEAN	    -> "Boolean"
    , CHAR	    -> "String"
    , CLOB	    -> "Clob"
    , DATALINK	    -> "URL"
    , DATE	    -> "Date"
    , DECIMAL	    -> "BigDecimal"
    , DOUBLE	    -> "Double"
    , FLOAT	    -> "Float"
    , INTEGER	    -> "Int"
    , JAVA_OBJECT   -> "Object"
    , LONGNVARCHAR  -> "String"
    , LONGVARBINARY -> "Bytes"
    , LONGVARCHAR   -> "String"
    , NCHAR	    -> "String"
    , NCLOB	    -> "NClob"
    , NUMERIC	    -> "BigDecimal"
    , NVARCHAR	    -> "String"
    , REAL	    -> "Float"
    , REF	    -> "Ref"
    , ROWID	    -> "RowId"
    , SMALLINT	    -> "Short"
    , SQLXML	    -> "SQLXML"
    , TIME	    -> "Time"
    , TIMESTAMP	    -> "Timestamp"
    , TINYINT	    -> "Byte"
    , VARBINARY	    -> "Bytes"
    , VARCHAR	    -> "String"
  )

  /* a mapping from getFoo/setFoo to their returned (scala) types; theoretically could be determined by reflection, but it's a bit messy */
  final val types = Map(
      "Array"     -> "java.sql.Array"
    , "BigDecimal"-> "java.math.BigDecimal"
    , "Blob"      -> "java.sql.Blob"
    , "Boolean"   -> "scala.Boolean"
    , "Byte"      -> "scala.Byte"
    , "Bytes"     -> "scala.Array[scala.Byte]" // XXX this may not work
    , "Clob"      -> "java.sql.Clob"
    , "Date"      -> "java.sql.Date"
    , "Double"    -> "scala.Double"
    , "Float"     -> "scala.Float"
    , "Int"       -> "scala.Int"
    , "Long"      -> "scala.Long"
    , "NClob"     -> "java.sql.NClob"
    , "Object"    -> "java.lang.Object"
    , "Ref"       -> "java.sql.Ref"
    , "RowId"     -> "java.sql.RowId"
    , "SQLXML"    -> "java.sql.SQLXML"
    , "Short"     -> "scala.Short"
    , "String"    -> "scala.String"
    , "Time"      -> "java.sql.Time"
    , "Timestamp" -> "java.sql.Timestamp"
    , "URL"       -> "java.net.URL"
  )

  def apply(id: Int, cls: => String) = {
    if (id == UnknownType.jdbcType)
      UnknownType
    else if (id == NullType.jdbcType)
      NullType
    else if (names.contains(id))
      JdbcType(id)
    else
      CustomType(id, cls)
  }

  final val Any = UnknownType
  final val Null = NullType
  final val Boolean = JdbcType(BOOLEAN)
  final val Int = JdbcType(INTEGER)
  final val Long = JdbcType(BIGINT)
  final val Double = JdbcType(DOUBLE)
  final val String = JdbcType(VARCHAR)
  final val Date = JdbcType(DATE)
  final val Timestamp = JdbcType(TIMESTAMP)
  final val Time = JdbcType(TIME)
}

private[sqltyped] object UnknownType extends Type(schemacrawler.schema.JavaSqlType.UNKNOWN.getJavaSqlType) {
  val name = "Object"
  val scalaType = "java.lang.Object"
}
private[sqltyped] object NullType extends Type(java.sql.Types.NULL) {
  val name = "Null" // not really but should never be used
  val scalaType = "Unit"
}
private[sqltyped] case class JdbcType(id: Int) extends Type(id) {
  def name = Type.names(id)
  def scalaType = Type.types(name)
}
private[sqltyped] case class CustomType(id: Int, mappedClass: String) extends Type(id) {
  def name = "Object"
  def scalaType = mappedClass
}

private[sqltyped] class MetaValue(val name: String, val tpe: Type, val nullable: Boolean)

private[sqltyped] class MetaStatement(val input: List[MetaValue], val output: List[MetaValue])

private[sqltyped] object Jdbc {
  def infer(db: DbConfig, sql: String): ?[MetaStatement] = 
    withConnection(db.getConnection) { conn =>
      val stmt = conn.prepareStatement(sql)
      for {
        out <- (Option(stmt.getMetaData) map inferOutput getOrElse Nil).ok
        in  <- Option(stmt.getParameterMetaData) map inferInput orFail "Input metadata not available"
      } yield new MetaStatement(in, out)
    } flatMap identity

  private def inferInput(meta: ParameterMetaData) = 
    (1 to meta.getParameterCount).toList map { i => 
      new MetaValue("a" + i, 
        try {
          Type(meta.getParameterType(i), meta.getParameterClassName(i))
        } catch {
          case e: SQLException => UnknownType
        },
        try {
          meta.isNullable(i) == ParameterMetaData.parameterNullable
        } catch {
          case e: SQLException => false
        })
    }

  private def inferOutput(meta: ResultSetMetaData) = 
    (1 to meta.getColumnCount).toList map { i => 
      new MetaValue(meta.getColumnName(i),
        try {
          Type(meta.getColumnType(i), meta.getColumnClassName(i))
        } catch {
          case e: SQLException => UnknownType
        },
        try {
          meta.isNullable(i) != ResultSetMetaData.columnNoNulls
        } catch {
          case e: SQLException => true
        })
    }

  def unknownTerm = Ast.Column("unknown", Ast.Table("unknown", None))

  def typeMetaStatement(meta: MetaStatement): TypedStatement = {
    def typeMetaValue(meta: MetaValue): TypedValue =
      TypedValue(meta.name, meta.tpe, meta.nullable, None, unknownTerm)
    val isQuery = !meta.output.isEmpty
    TypedStatement(meta.input.map(typeMetaValue _), meta.output.map(typeMetaValue _), 
      isQuery, Map(), Nil, if (isQuery) NumOfResults.Many else NumOfResults.One)
  }

  def withConnection[A](conn: Connection)(a: Connection => A): ?[A] = try { 
    a(conn).ok
  } catch {
    case e: SQLException => fail(e.getMessage)
  } finally { 
    conn.close 
  }
}
