package com.tomharrisonjr

/**
  * Created by tharrison on 4/11/16.
  */
sealed abstract class Player

case object X extends Player

case object O extends Player

case object Blank extends Player {
  override def toString = " "
}
