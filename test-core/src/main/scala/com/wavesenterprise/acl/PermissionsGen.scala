package com.wavesenterprise.acl

import com.wavesenterprise.acl.Role._
import com.wavesenterprise.utils.NTP
import org.scalacheck.Gen

object PermissionsGen {

  private val time = NTP(Seq("pool.ntp.org"))(monix.execution.Scheduler.global)

  val roleGen: Gen[Role] = Gen.oneOf(Miner, Issuer, Dexer, Permissioner, Blacklister, ContractDeveloper, ConnectionManager, ContractValidator)

  val nonEmptyRoleGen: Gen[NonEmptyRole] = Gen.oneOf(Role.values.collect { case role: NonEmptyRole => role })

  val allowedEmptyRoleGen: Gen[Role] = Gen.oneOf(Role.values.filterNot(_.isInstanceOf[NonEmptyRole]))

  val allowedEmptyRoleWithoutSenderGen: Gen[Role] = Gen.oneOf(
    Role.values.filterNot {
      case _: NonEmptyRole | Role.Sender => true
      case _                             => false
    }
  )

  val permissionOpTypeGen: Gen[OpType] = Gen.oneOf(OpType.Add, OpType.Remove)

  val timestampGen: Gen[Long] = Gen.choose(1, Long.MaxValue - 100)

  val ntpTimestampGen: Gen[Long] = Gen.choose(1, 1000).map(time.correctedTime() - _)

  val permissionOpGen: Gen[PermissionOp] = for {
    permission <- roleGen
    timestamp  <- timestampGen
    opType     <- permissionOpTypeGen
    dueDateOpt <- Gen.option(timestampGen)
  } yield PermissionOp(opType, permission, timestamp, dueDateOpt)

  def permissionOpGen(issuedBefore: Long, dueAfter: Long, dueBefore: Long): Gen[PermissionOp] =
    for {
      permission <- roleGen
      timestamp  <- timestampGen.retryUntil(_ < issuedBefore)
      opType     <- permissionOpTypeGen
      dueDate <- timestampGen
        .retryUntil(ts => ts != dueAfter && ts != dueBefore)
        .map(ts => ts % (dueBefore - dueAfter) + dueAfter)
    } yield PermissionOp(opType, permission, timestamp, Some(dueDate))

  val permissionOpAddGen: Gen[PermissionOp] =
    permissionOpGen.map(_.copy(opType = OpType.Add))

  val permissionOpWithoutDueGen: Gen[PermissionOp] =
    permissionOpGen.map(_.copy(dueTimestampOpt = None))

  val permissionOpAddWithoutDueGen: Gen[PermissionOp] =
    permissionOpWithoutDueGen.map(_.copy(opType = OpType.Add))

  val permissionOpsGen: Gen[List[PermissionOp]] =
    Gen.listOf(permissionOpGen)

  val permissionOpsWithoutDueGen: Gen[List[PermissionOp]] =
    Gen.listOf(permissionOpWithoutDueGen)

  val permissionsGen: Gen[Permissions] =
    for {
      ops <- permissionOpsGen
    } yield Permissions(ops)

  val minerPermissionsGen: Gen[Permissions] = for {
    ops <- permissionOpsGen
  } yield Permissions(ops.map(_.copy(role = Role.Miner)))

  case class CorrespondingTimestamps(before: Long, after: Long)

  val correspondingTimestampsGen: Gen[CorrespondingTimestamps] = for {
    t1 <- ntpTimestampGen
    t2 <- ntpTimestampGen.retryUntil(_ != t1)
  } yield CorrespondingTimestamps(before = Math.min(t1, t2), after = Math.max(t1, t2))
}
