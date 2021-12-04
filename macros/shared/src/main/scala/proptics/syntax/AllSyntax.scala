package proptics.syntax

import proptics.syntax.applied.AppliedSyntax
import proptics.syntax.macros.MacroSyntax

trait AllSyntax extends CoreSyntax with AppliedSyntax with MacroSyntax

object all extends AllSyntax
