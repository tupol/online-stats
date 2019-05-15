package org.tupol.stats

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.tupol.stats.optionops._

class OptionOpsProps extends Properties("Option[Double] Ops") {

  property("Some + double should be Some double") =
    forAll { (x: Double, y: Double) =>
      Some(x) + y == Some(x + y)
    }

  property("Some - double should be Some double") =
    forAll { (x: Double, y: Double) =>
      Some(x) - y == Some(x - y)
    }

  property("Some * double should be Some double") =
    forAll { (x: Double, y: Double) =>
      Some(x) * y == Some(x * y)
    }

  property("Some + None should be Some double") =
    forAll { (x: Double) =>
      Some(x) + None == Some(x)
    }

  property("Some - None should be Some double") =
    forAll { (x: Double) =>
      Some(x) - None == Some(x)
    }

  property("Some * None should be Some 0") =
    forAll { (x: Double) =>
      Some(x) * None == Some(0)
    }

  property("None + Some should be Some double") =
    forAll { (x: Double) =>
      None + Some(x) == Some(x)
    }

  property("None - Some should be Some double") =
    forAll { (x: Double) =>
      None - Some(x) == Some(-x)
    }

  property("None * Some should be Some 0") =
    forAll { (x: Double) =>
      None * Some(x) == Some(0)
    }

  property("Some / 2.0 should be Some double") =
    forAll { (x: Double) =>
      Some(x) / 2.0 == Some(x / 2.0)
    }

  property("Some / 0.0 should be None") =
    forAll { (x: Double, y: Double) =>
      Some(x) / 0.0 == None
    }

  property("Some < double should be same as <") =
    forAll { (x: Double, y: Double) =>
      (Some(x) < y) == Some(x < y)
    }

  property("Some <= double should be same as <=") =
    forAll { (x: Double, y: Double) =>
      (Some(x) <= y) == Some(x <= y)
    }

  property("Some > double should be same as >") =
    forAll { (x: Double, y: Double) =>
      (Some(x) > y) == Some(x > y)
    }

  property("Some >= double should be same as >=") =
    forAll { (x: Double, y: Double) =>
      (Some(x) >= y) == Some(x >= y)
    }

  property("Some < None should be None") =
    forAll { (x: Double, y: Double) =>
      (Some(x) < None) == None
    }

  property("Some <= None should be None") =
    forAll { (x: Double, y: Double) =>
      (Some(x) <= None) == None
    }

  property("Some > None should be None") =
    forAll { (x: Double, y: Double) =>
      (Some(x) > None) == None
    }

  property("Some >= None should be None") =
    forAll { (x: Double, y: Double) =>
      (Some(x) >= None) == None
    }
}
