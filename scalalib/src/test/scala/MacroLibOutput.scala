package mdf.macrolib.test

import mdf.macrolib._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import play.api.libs.json._

// Output tests (Scala -> JSON).
// TODO: unify these tests with the input tests?

// Tests for filler macros.
class FillerMacroOutput extends FlatSpec with Matchers {
  "Valid lvt macro" should "be generated" in {
    val expected = """
    | {
    |   "type": "filler cell",
    |   "name": "MY_FILLER_CELL",
    |   "vt": "lvt"
    | }
    |""".stripMargin
    FillerMacro(Filler, "MY_FILLER_CELL", "lvt").toJSON shouldBe Json.parse(expected)
  }

  "Valid metal macro" should "be generated" in {
    val expected = """
    | {
    |   "type": "metal filler cell",
    |   "name": "METAL_FILLER_CELL",
    |   "vt": "lvt"
    | }
    |""".stripMargin
    FillerMacro(MetalFiller, "METAL_FILLER_CELL", "lvt").toJSON shouldBe Json.parse(expected)
  }

  "Valid hvt macro" should "be generated" in {
    val expected = """
    | {
    |   "type": "filler cell",
    |   "name": "HVT_CELL_PROP",
    |   "vt": "hvt"
    | }
    |""".stripMargin
    FillerMacro(Filler, "HVT_CELL_PROP", "hvt").toJSON shouldBe Json.parse(expected)
  }
}

class SRAMPortOutput extends FlatSpec with Matchers {
  "Extra port" should "be generated" in {
    val m = MacroExtraPort(
      name="TIE_HIGH",
      width=8,
      portType=Constant,
      value=((1 << 8) - 1)
    )
    val expected = """
    | {
    |   "type": "constant",
    |   "name": "TIE_HIGH",
    |   "width": 8,
    |   "value": 255
    | }
    |""".stripMargin
    m.toJSON shouldBe Json.parse(expected)
  }

  "Minimal write port" should "be generated" in {
    val m = MacroPort(
      address=PolarizedPort(name="addr", polarity=ActiveHigh),
      clock=PolarizedPort(name="clk", polarity=PositiveEdge),

      writeEnable=Some(PolarizedPort(name="write_enable", polarity=ActiveHigh)),

      input=Some(PolarizedPort(name="data_in", polarity=ActiveHigh)),

      width=32, depth=1024 // These numbers don't matter.
    )
    val expected = """
    | {
    |   "address port name": "addr",
    |   "address port polarity": "active high",
    |   "clock port name": "clk",
    |   "clock port polarity": "positive edge",
    |   "write enable port name": "write_enable",
    |   "write enable port polarity": "active high",
    |   "input port name": "data_in",
    |   "input port polarity": "active high"
    | }
    |""".stripMargin
    m.toJSON shouldBe Json.parse(expected)
  }

  "Minimal read port" should "be generated" in {
    val m = MacroPort(
      address=PolarizedPort(name="addr", polarity=ActiveHigh),
      clock=PolarizedPort(name="clk", polarity=PositiveEdge),

      output=Some(PolarizedPort(name="data_out", polarity=ActiveHigh)),

      width=32, depth=1024 // These numbers don't matter.
    )
    val expected = """
    | {
    |   "address port name": "addr",
    |   "address port polarity": "active high",
    |   "clock port name": "clk",
    |   "clock port polarity": "positive edge",
    |   "output port name": "data_out",
    |   "output port polarity": "active high"
    | }
    |""".stripMargin
    m.toJSON shouldBe Json.parse(expected)
  }

  "Masked read port" should "be generated" in {
    val m = MacroPort(
      address=PolarizedPort(name="addr", polarity=ActiveHigh),
      clock=PolarizedPort(name="clk", polarity=PositiveEdge),

      output=Some(PolarizedPort(name="data_out", polarity=ActiveHigh)),

      maskPort=Some(PolarizedPort(name="mask", polarity=ActiveHigh)),
      maskGran=Some(8),

      width=32, depth=1024 // These numbers don't matter.
    )
    val expected = """
    | {
    |   "address port name": "addr",
    |   "address port polarity": "active high",
    |   "clock port name": "clk",
    |   "clock port polarity": "positive edge",
    |   "output port name": "data_out",
    |   "output port polarity": "active high",
    |   "mask port name": "mask",
    |   "mask port polarity": "active high",
    |   "mask granularity": 8
    | }
    |""".stripMargin
    m.toJSON shouldBe Json.parse(expected)
  }

  "Everything port" should "be generated" in {
    val m = MacroPort(
      address=PolarizedPort(name="addr", polarity=ActiveHigh),
      clock=PolarizedPort(name="clk", polarity=PositiveEdge),

      writeEnable=Some(PolarizedPort(name="write_enable", polarity=ActiveHigh)),
      readEnable=Some(PolarizedPort(name="read_enable", polarity=ActiveHigh)),
      chipEnable=Some(PolarizedPort(name="chip_enable", polarity=ActiveHigh)),

      output=Some(PolarizedPort(name="data_out", polarity=ActiveHigh)),
      input=Some(PolarizedPort(name="data_in", polarity=ActiveHigh)),

      maskPort=Some(PolarizedPort(name="mask", polarity=ActiveHigh)),
      maskGran=Some(8),

      width=32, depth=1024 // These numbers don't matter.
    )
    val expected = """
    | {
    |   "address port name": "addr",
    |   "address port polarity": "active high",
    |   "clock port name": "clk",
    |   "clock port polarity": "positive edge",
    |   "write enable port name": "write_enable",
    |   "write enable port polarity": "active high",
    |   "read enable port name": "read_enable",
    |   "read enable port polarity": "active high",
    |   "chip enable port name": "chip_enable",
    |   "chip enable port polarity": "active high",
    |   "output port name": "data_out",
    |   "output port polarity": "active high",
    |   "input port name": "data_in",
    |   "input port polarity": "active high",
    |   "mask port name": "mask",
    |   "mask port polarity": "active high",
    |   "mask granularity": 8
    | }
    |""".stripMargin
    m.toJSON shouldBe Json.parse(expected)
  }
}

class SRAMMacroOutput extends FlatSpec with Matchers {
    val m = SRAMMacro(
      macroType=SRAM,
      name="awesome_mem",
      width=32,
      depth=1024,
      family="1rw",
      ports=Seq(MacroPort(
        address=PolarizedPort(name="addr", polarity=ActiveHigh),
        clock=PolarizedPort(name="clk", polarity=PositiveEdge),

        writeEnable=Some(PolarizedPort(name="write_enable", polarity=ActiveHigh)),
        readEnable=Some(PolarizedPort(name="read_enable", polarity=ActiveHigh)),
        chipEnable=Some(PolarizedPort(name="chip_enable", polarity=ActiveHigh)),

        output=Some(PolarizedPort(name="data_out", polarity=ActiveHigh)),
        input=Some(PolarizedPort(name="data_in", polarity=ActiveHigh)),

        maskPort=Some(PolarizedPort(name="mask", polarity=ActiveHigh)),
        maskGran=Some(8),

        width=32, depth=1024 // These numbers don't matter.
      )),
      extraPorts=List()
    )
    val expected = """
    | {
    |   "type": "sram",
    |   "name": "awesome_mem",
    |   "width": 32,
    |   "depth": 1024,
    |   "family": "1rw",
    |   "ports": [
    | {
    |   "address port name": "addr",
    |   "address port polarity": "active high",
    |   "clock port name": "clk",
    |   "clock port polarity": "positive edge",
    |   "write enable port name": "write_enable",
    |   "write enable port polarity": "active high",
    |   "read enable port name": "read_enable",
    |   "read enable port polarity": "active high",
    |   "chip enable port name": "chip_enable",
    |   "chip enable port polarity": "active high",
    |   "output port name": "data_out",
    |   "output port polarity": "active high",
    |   "input port name": "data_in",
    |   "input port polarity": "active high",
    |   "mask port name": "mask",
    |   "mask port polarity": "active high",
    |   "mask granularity": 8
    | }
    |   ]
    | }
    |""".stripMargin
    m.toJSON shouldBe Json.parse(expected)
}
