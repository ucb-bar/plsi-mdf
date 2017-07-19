package mdf.macrolib

import java.io.File
import play.api.libs.json._
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

// TODO: decide if we should always silently absorb errors

// See macro_format.yml for the format description.

// Macro type
sealed abstract class MacroType
case object SRAM extends MacroType
case object Filler extends MacroType
case object MetalFiller extends MacroType
case object NoType extends MacroType
object MacroType {
  implicit def toMacroType(s: Any): MacroType = {
    s match {
      case "sram" => SRAM
      case "filler cell" => Filler
      case "metal filler cell" => MetalFiller
      case _ => NoType
    }
  }

  implicit def toString(t: MacroType): String = {
    t match {
      case SRAM => "sram"
      case Filler => "filler cell"
      case MetalFiller => "metal filler cell"
      case _ => ""
    }
  }
}

// "Base class" for macros
abstract class Macro {
  // Get rid of this field entirely, since type of macro is determined by subclass?
  def macroType: MacroType
  def name: String
}

// Filler and metal filler
case class FillerMacro(macroType: MacroType, name: String, vt: String) extends Macro {
  override def toString(): String = {
    s"FillerMacro(macroType=${macroType}, name=${name}, vt=${vt})"
  }

  def toJSON(): JsObject = {
    JsObject(Seq(
      "type" -> JsString(MacroType.toString(macroType)),
      "name" -> Json.toJson(name),
      "vt" -> Json.toJson(vt)
    ))
  }
}
object FillerMacro {
  def parseJSON(macroType: MacroType, json:Map[String, JsValue]): Option[FillerMacro] = {
    require(macroType == Filler || macroType == MetalFiller)
    val name: String = json.get("name") match {
      case Some(x:JsString) => x.value match {
        case "" => return None
        case x => x
      }
      case _ => return None
    }
    val vt: String = json.get("vt") match {
      case Some(x:JsString) => x.value match {
        case "" => return None
        case x => x
      }
      case _ => return None
    }
    Some(FillerMacro(macroType, name, vt))
  }
}

// SRAM macro
case class SRAMMacro(macroType: MacroType, name: String,
  width: Int,
  depth: Int,
  family: String,
  ports: Seq[MacroPort],
  extraPorts: Seq[MacroExtraPort]
) extends Macro {
  def toJSON(): JsObject = {
    val output = new ListBuffer[(String, JsValue)]()
    output.appendAll(Seq(
      "type" -> JsString(MacroType.toString(macroType)),
      "name" -> Json.toJson(name),
      "width" -> Json.toJson(width),
      "depth" -> Json.toJson(depth),
      "ports" -> JsArray(ports map { _.toJSON })
    ))
    if (family != "") {
      output.appendAll(Seq("family" -> Json.toJson(family)))
    }
    if (extraPorts.length > 0) {
      output.appendAll(Seq("extra ports" -> JsArray(extraPorts map { _.toJSON })))
    }

    JsObject(output)
  }
}
object SRAMMacro {
  def parseJSON(json:Map[String, JsValue]): Option[SRAMMacro] = {
    val name: String = json.get("name") match {
      case Some(x:JsString) => x.as[String]
      case _ => return None
    }
    val width: Int = json.get("width") match {
      case Some(x:JsNumber) => x.value.intValue
      case _ => return None
    }
    val depth: Int = json.get("depth") match {
      case Some(x:JsNumber) => x.value.intValue
      case _ => return None
    }
    val family: String = json.get("family") match {
      case Some(x:JsString) => x.as[String]
      case _ => "" // optional
    }
    val ports: Seq[MacroPort] = json.get("ports") match {
      case Some(x:JsArray) => x.as[List[Map[String, JsValue]]] map { a => { val b = MacroPort.parseJSON(a, width, depth); if (b == None) { return None } else b.get } }
      case _ => List()
    }
    if (ports.length == 0) {
      // Can't have portless memories.
      return None
    }
    val extraPorts: Seq[MacroExtraPort] = json.get("extra ports") match {
      case Some(x:JsArray) => x.as[List[Map[String, JsValue]]] map { a => { val b = MacroExtraPort.parseJSON(a); if (b == None) { return None } else b.get } }
      case _ => List()
    }
    Some(SRAMMacro(SRAM, name, width, depth, family, ports, extraPorts))
  }
}

// Type of extra port
sealed abstract class MacroExtraPortType
case object Constant extends MacroExtraPortType
object MacroExtraPortType {
  implicit def toMacroExtraPortType(s: Any): Some[MacroExtraPortType] = {
    s match {
      case "constant" => Some(Constant)
      case _ => None
    }
  }

  implicit def toString(t: MacroExtraPortType): String = {
    t match {
      case Constant => "constant"
      case _ => ""
    }
  }
}

// Extra port in SRAM
case class MacroExtraPort(
  name: String,
  width: Int,
  portType: MacroExtraPortType,
  value: BigInt
) {
  def toJSON(): JsObject = {
    JsObject(Seq(
      "name" -> Json.toJson(name),
      "width" -> Json.toJson(width),
      "type" -> JsString(MacroExtraPortType.toString(portType)),
      "value" -> JsNumber(BigDecimal(value))
    ))
  }
}
object MacroExtraPort {
  def parseJSON(json:Map[String, JsValue]): Option[MacroExtraPort] = {
    val name = json.get("name") match {
      case Some(x:JsString) => x.value
      case _ => return None
    }
    val width = json.get("width") match {
      case Some(x:JsNumber) => x.value.intValue
      case _ => return None
    }
    val portType: MacroExtraPortType = json.get("type") match {
      case Some(x:JsString) => MacroExtraPortType.toMacroExtraPortType(x.value) match {
        case Some(t:MacroExtraPortType) => t
        case _ => return None
      }
      case _ => return None
    }
    val value = json.get("value") match {
      case Some(x:JsNumber) => x.value.toBigInt
      case _ => return None
    }
    Some(MacroExtraPort(name, width, portType, value))
  }
}

// A named port that also has polarity.
case class PolarizedPort(name: String, polarity: PortPolarity) {
  def toSeqMap(prefix: String): Seq[Tuple2[String, JsValue]] = {
    Seq(
      prefix + " port name" -> Json.toJson(name),
      prefix + " port polarity" -> JsString(polarity)
    )
  }
}
object PolarizedPort {
  // Parse a pair of "<prefix> port name" and "<prefix> port polarity" keys into a
  // polarized port definition.
  def parseJSON(json:Map[String, JsValue], prefix: String): Option[PolarizedPort] = {
    val name = json.get(prefix + " port name") match {
      case Some(x:JsString) => Some(x.value)
      case _ => None
    }
    val polarity: Option[PortPolarity] = json.get(prefix + " port polarity") match {
      case Some(x:JsString) => Some(x.value)
      case _ => None
    }

    (name, polarity) match {
      case (Some(n:String), Some(p:PortPolarity)) => Some(PolarizedPort(n, p))
      case _ => None
    }
  }
}

// A SRAM memory port
case class MacroPort(
  address: PolarizedPort,
  clock: PolarizedPort,

  writeEnable: Option[PolarizedPort] = None,
  readEnable: Option[PolarizedPort] = None,
  chipEnable: Option[PolarizedPort] = None,

  output: Option[PolarizedPort] = None,
  input: Option[PolarizedPort] = None,

  maskPort: Option[PolarizedPort] = None,
  maskGran: Option[Int] = None,

  // For internal use only; these aren't port-specific.
  width: Int,
  depth: Int
  ) {
  val effectiveMaskGran = maskGran.getOrElse(width)

  def toJSON(): JsObject = {
    val keys: Seq[Tuple2[String, Option[Any]]] = Seq(
      "address" -> Some(address),
      "clock" -> Some(clock),
      "write enable" -> writeEnable,
      "read enable" -> readEnable,
      "chip enable" -> chipEnable,

      "output" -> output,
      "input" -> input,

      "mask" -> maskPort,
      "mask granularity" -> maskGran
    )
    JsObject(keys.flatMap(k => {
      val (key, value) = k
      value match {
        case Some(x:Int) => Seq(key -> JsNumber(x))
        case Some(x:PolarizedPort) => x.toSeqMap(key)
        case _ => List()
      }
    }))
  }
}
object MacroPort {
  def parseJSON(json:Map[String, JsValue], width:Int, depth:Int): Option[MacroPort] = {
    val address = PolarizedPort.parseJSON(json, "address")
    val clock = PolarizedPort.parseJSON(json, "clock")
    if (address == None || clock == None) {
      return None
    }

    // TODO: validate based on family (e.g. 1rw must have a write enable, etc)
    val writeEnable = PolarizedPort.parseJSON(json, "write enable")
    val readEnable = PolarizedPort.parseJSON(json, "read enable")
    val chipEnable = PolarizedPort.parseJSON(json, "chip enable")

    val output = PolarizedPort.parseJSON(json, "output")
    val input = PolarizedPort.parseJSON(json, "input")

    val maskPort = PolarizedPort.parseJSON(json, "mask")
    val maskGran: Option[Int] = json.get("mask granularity") match {
      case Some(x:JsNumber) => Some(x.value.intValue)
      case _ => None
    }

    Some(MacroPort(width=width, depth=depth,
      address=address.get,
      clock=clock.get,

      writeEnable=writeEnable,
      readEnable=readEnable,
      chipEnable=chipEnable,

      output=output,
      input=input,

      maskPort=maskPort,
      maskGran=maskGran
    ))
  }
}

// Port polarity
trait PortPolarity
case object ActiveLow extends PortPolarity
case object ActiveHigh extends PortPolarity
case object NegativeEdge extends PortPolarity
case object PositiveEdge extends PortPolarity
object PortPolarity {
  implicit def toPortPolarity(s: String): PortPolarity = (s: @unchecked) match {
    case "active low" => ActiveLow
    case "active high" => ActiveHigh
    case "negative edge" => NegativeEdge
    case "positive edge" => PositiveEdge
  }
  implicit def toPortPolarity(s: Option[String]): Option[PortPolarity] =
    s map toPortPolarity

  implicit def toString(p: PortPolarity): String = {
    p match {
      case ActiveLow => "active low"
      case ActiveHigh => "active high"
      case NegativeEdge => "negative edge"
      case PositiveEdge => "positive edge"
    }
  }
}

object Utils {
  // Read a MDF file from a String.
  def readMDFFromString(str: String): Option[Seq[Macro]] = {
    Json.parse(str) match {
      // Make sure that the document is a list.
      case arr:JsArray => {
        val result: List[Option[Macro]] = arr.as[List[Map[String, JsValue]]] map { obj =>
          // Check the type of object.
          val objTypeStr: String = obj.get("type") match {
            case Some(x:JsString) => x.as[String]
            case _ => return None // error, no type found
          }
          val objType: MacroType = objTypeStr

          objType match {
            case Filler | MetalFiller => FillerMacro.parseJSON(objType, obj)
            case SRAM => SRAMMacro.parseJSON(obj)
            case NoType => None // skip unknown macro types
          }
        }
        // Remove all the Nones and convert back to Seq[Macro]
        Some(result.filter { x => x != None } map { x => x.get })
      }
      case _ => None
    }
  }

  // Read a MDF file from a path.
  def readMDFFromPath(path: Option[String]): Option[Seq[Macro]] = {
    path match {
      case None => None
      // Read file into string and parse
      case Some(p) => Utils.readMDFFromString(scala.io.Source.fromFile(p).mkString)
    }
  }
}