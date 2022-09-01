package com.wavesenterprise.lang

import com.wavesenterprise.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.wavesenterprise.lang.WavesGlobal // Hack for IDEA
}
