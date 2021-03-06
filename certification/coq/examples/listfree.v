From mathcomp
Require Import ssreflect ssrbool ssrnat eqtype seq ssrfun.
From fcsl
Require Import prelude pred pcm unionmap heap.
From HTT
Require Import stmod stsep stlog stlogR.
From SSL
Require Import core.

Inductive lseg (x : ptr) (s : seq nat) (h : heap) : Prop :=
| lseg0 of x == 0 of
    s = nil /\ h = empty
| lseg1 of x != 0 of
  exists nxt s1 v,
  exists h',
    s = [:: v] ++ s1 /\ h = x :-> v \+ x .+ 1 :-> nxt \+ h' /\ lseg nxt s1 h'
.
Definition listfree_type :=
  forall (x : ptr),
  {(S : seq nat)},
    STsep (
      fun h =>
          lseg x S h,
      [vfun (_: unit) h =>
          h = empty      ]).
Program Definition listfree : listfree_type :=
  Fix (fun (listfree : listfree_type) x =>
    Do (
  if x == 0
  then
    ret tt
  else
    nxtx2 <-- @read ptr (x .+ 1);
    listfree nxtx2;;
    dealloc (x .+ 1);;
    dealloc x;;
    ret tt
    )).
Next Obligation.
  (* pull out precondition ghosts *)
  (* apply: ghR. *)

  (* move precondition heap to context *)
  (* move=>h//=. *)

  ssl_ghostelim_pre.

  (* move precondition ghosts to context *)
  move=>S.

  (* move precondition heaplets to context *)
  move=>H_lseg.

  (* move heap validity assertion generated by ghR to context *)
  move=>H_valid.
  
  (* [Open][AbduceBranch] Make subgoals out of each conditional branch *)
  case: ifP=>Hcond.
  - (* [Open] match current condition to correct constructor *)
    case: H_lseg; rewrite Hcond=>//= _.

    (* move constructor's pure part to context *)
    move=>[lseg_phi].

    (* substitute constructor's spatial part (heap) to conclusion *)
    move=>[->].

    (* ret tt *)
    ssl_emp.

  - (* [Open] match current condition to correct constructor *)
    case: H_lseg; rewrite Hcond=>//= _.

    (* move constructor's value existentials to context *)
    move=>[nxt] [s1] [v].

    (* move constructor's heap existentials to context *)
    move=>[h'].

    (* move constructor's pure part to context *)
    move=>[lseg_phi].

    (* substitute constructor's spatial part (heap) to conclusion *)
    move=>[->].

    (* move constructor's predicate part to context *)
    move=>H_rec_lseg.

    (* nxtx2 <-- @read ptr (x .+ 1); *)
    ssl_read.

    (* listfree nxtx2;; *)
    put_to_head h'.
    apply: bnd_seq.
    apply: (gh_ex s1).
    apply: val_do=>//= _ ? ->; rewrite unitL=>_.

    (* dealloc x.+1;; *)
    ssl_dealloc.

    (* dealloc x;; *)
    ssl_dealloc.

    (* ret tt *)
    ssl_emp.
Qed.
