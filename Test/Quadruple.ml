type ('a,'b,'c,'d) quadruple =
{
  val32: 'a;
  val16: 'b;
  val8: 'c;
  val1: 'd;
}

let select_quad q = function
| S32 -> q.val32
| S16 -> q.val16
| S8  -> q.val8
| S1  -> q.val1

let update_quad q v = function
| S32 -> { q with val32 = v; }
| S16 -> { q with val16 = v; }
| S8  -> { q with val8  = v; }
| S1  -> { q with val1  = v; }

