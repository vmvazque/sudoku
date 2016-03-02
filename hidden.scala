package scala.reddit.hiddenstring

// import scala.math.Ordering.Implicits._

object Hidden extends App {
  import Util._
  type Tup = Tuple2[Char, Int]

  println("Hello World")
  val input = "ttvmswxjzdgzqxotby_lslonwqaipchgqdo_yz_fqdagixyrobdjtnl_jqzpptzfcdcjjcpjjnnvopmh";
  val input2 = "aabcbded"

  val challenge = """hpevfwqjmjryhemuqjoiatpjmddxdjwzskdcfgdtbmkbcxrnmjuoyddnqwluimjwvguxehszxzvbmufq
lrepncxxbrrzxnzmkoyhrjcstvfazyhrhgssximjdfcmdjusylfkwbedyrsxovrmvjzaljfjmywpfnjg
isoqbdyspgzlcmdjmhbpxhzvvhckidzuwzkauffsujmcrhvgeqvasjakgtzlxkthjqwxypmsovjbfshr
rxtdvkmbyhejoeydnrdowuwhgmbvxmpixyttglsjgmcoqbberssfjraaqfrkmebsozsjfnubhktbbai_
vxbifbofyednnutmxtisvfsktbqfijfzdjoqybuohtztysqelaqyixyaiolbgwylwfisfwubivuoablx
smrqggedwyiqvseevwbcxcfjttdbweedcjgnsorizflsjtmltcoaynsrsupavqwcyzhgiplwkohlhrai
nazaacvuqblpbzimgoxirejbshnbmdtgsbvlhpnugggencjaczqqiwixrwiyobmlkbwdlwcioqmjhoac
dvcqdypxeichmgywocbcafumthdqrbjnpgnnmaasxiaxxfymcyiuqduztqneodstbcnjpeebgxgosoyd
vpzlqjuroebbehafsemanwprhwkircuhlgcftqsjdusrqetbthxclfokpdlspxzuvhxpbeqqbfpqffsg
yilqltfxrmtimcugytazkerhcfnirtavcnmfdyictlncwttkmxyfhgejygfefqrjknuqsfldmjmwjdfq
sicfrzbfazchdgznekwmhridelcejnkmcgmpgtihbwmplrtrrefoyhyzxpjjlkabbbgspeokzhpjxsvp
fjmdsoripvfrgyzxodoeirwwdaofdmwqrqyvdijlfqyzfspdoyrhewxbpufdqcpqdolkmrnvedixzpfd
akggkslxcrjbrmnynviihbkzaqqffkkcgwjbettexhlwlasdfjnslwsmnclhafvebxxfdozsjtdvobik
rrsuysujwliobagobxmlyxjeltwzwxpyrnkdxfemotfncyriaycyfemygjmpboocgtsvttqntegvleyn
wgpjhyyysbltoxljsascsngbgfqmpzgpejzlmdkjzzlfxvagyrasmpzqntgqsvyqjugkhbrbkiqewlyf
tvsq_______znp_____xkwt______wef______tz______kfc_______ha_______pn__lmg__iakrbt
iyfi__uojrxvx__tps__fp__pfpndbi__ggpalde__wmd__kn__ifiadob__hdljdbd__zl__whlwilt
bcmt__haagmjg__dwx__oh__utnzudq__xstxxyc__vly__mr__viilzav__swosyvc__i__hnaqxyev
jykc__wyfoyir__ewp__ij__mrdavxl__tcdtxqy__fnr__cf__mrkepwj__djhrsau____lhefqxgmu
zdgf______tjg__fip__mi__b____xc__vjvhpqy______vff_____wuup_____kqct___htiggvvpet
yvco__pqbrlox__ayj__af__dnn__kx__mlitytx____jauna__kncmiym__dlwushk____gjptzccgc
nntt__hfqyxzi__eqn__vz__hlh__we__dtfkfvf__g__litm__zeqjtdl__bkdapxs__o__oxeouwer
bfjr__ipcqmop__kec__ip__icc__ci__vpxxueu__eq__sau__nhheydy__efqkdgq__us__pzlndhk
hdmk__cmfvzwcb_____xdka______trj______yj__xpi__he_______nb_______by__rrn__tvxvig
jfpseyjjbrrtsfnmbrokdqtfzhhdtbhtvpiyshmvcqaypfxcvbgvbvwrkanjfcsjnanmktkwimnvynuk
cmgtqmovkrdmfuduqvbqydagsttictcnsrhfrpoebcehdzhjamykqpjtktufcvokljjijjsrivyhxtgw
ojgoujyhmekzsoczwlqnruwcuhudgfaijzrkewzgjvorsmabpcdmurctwjrddcnkmfvabjwlbqssihdy
bgfqchqdvjcsdllrlwmyikuvthguzfbgocaeqktvbcapzdcfjphqnhundtljqjeyfrkjspfvghqddxwx
idtjjkctrkfcjmdpqyvavqbntpmkkuswfgbgalrysjfnzezjjscahoodjjelavydefzjmhsqfufsexlv
vzziymsyqrcvhsrxjnysioswvjlqdbnwgyjlanmhzkbygkptycdoifsibytbrixggjeiepaybzxhvfsy
ayeptgpxbhhfkkpromhjykfxnujorlzcmkcmvvgmveyfkgiwgosznfpmbhixsakxfkuxhwcgularehpa
guquulrjllxmkfzgnchrxzcfdklytpfnezergkwkhgalqlvdhkdgulgfaxtybqttcjtlgmfwaymaxlwa
spyrboibwkzzbtgigyswbtpwxgphcmkfpmvbfjimnxctinqssshofhlvlpqcwiuacjyxyqmvaibezofv
atyhpqvjubgcwqeoytloypjphoxeimumuvswxkgamodoxiciwmgxvsenkgdhttzlenjbszrksopicjcj
nvsosrapkfilwsaoptdavlfglioqpwoqskbgikksnnuzvmxyrtrbjouvgokxgbnwxnivtykvhjkaydsk
zoowbhjrlojgeecdoggqqtomcdgrjzmlkhubyaewwtrlyutsptdrrigopueicoganyasrjeaiivzairu
lklovyrpckwpowprxtvhaeivpudfchxbwvtosmivpcsesbzpsynxitlisuifuehceonjeydljzuzpsgj
llcywoxbblitscquxiykcjxhsgkbhfhfrshsrpyrcaetahuwbeybvlvkthxydkapxlfikdwudjkmjjsa
zajxpuikiqwsifhldfovqoycwmtlmcaycirhcehxnpfadrgyaogpcmomcgtmacnvbwfnimaqqvxijcbp
mckwimloiinindfuakqjmpyjisxnbybtywhymnkdoyiphijzelmrazplgfcmcsjiovxqdxmuqulzklgx"""

  lazy val asOne = challenge.split("\n").mkString("")
  val t0 = System.nanoTime()
  println("result: " + doOther(asOne))
  val t1 = System.nanoTime()
  println("Elapsed Time: " + (t1 - t0) + " ns")
  println("In seconds: " + ((t1 - t0)/1000000000) + " s")


  //current implentation has runtime of 84s, i think best was 69
  def getLastSub(searched: Tup, s: Seq[Tup]) = {
    lazy val end = s match {
      case Seq() => searched._2
      case _ => s.last._2
    }

    s.lastIndexWhere(_._1 == searched._1) match {
      case -1 => new UniqueThing(searched._1, searched._2, end, end, false)
      case x => new UniqueThing(searched._1, searched._2, s(x)._2, s.last._2, true)
    }
  }

  //as a 'shortcut' only recompute letters where index < removed.end || letter == removed.letter
  //if > than end then just sub start and end by 2
  //define fileter method,
  //for more a shortcut  keep a reference of where the duplicate happened
  //if duplicate not between end, then keep?
  // def filter(r: Option[UniqueThing])(tup: Tup) = {
  //   r match {
  //     case Some(x) => tup._2 <= x.end -2 || tup._1 == x.letter
  //     case None => true
  //   }
  // }

  // def filter(l: Seq[UniqueThing])(tup: Tup) = {
  //   l.view.filter
  // }

  def findAllMissing(s: String, l: Seq[UniqueThing]) = {
    val f = findLongest(s)_
    // val filt = filter(l)_
    val filt = l.map(_.start)
    s.zipWithIndex.withFilter(x => !filt.contains(x._2)).map(f(_))
  }

  def findLongest(s: String)(search: Tuple2[Char, Int]) = {
    def doRec(acc: Seq[Tup], seen: Set[Char], toParse: Seq[Tup]): UniqueThing = {
      //logic should be:
      //if no more to parse, get last sub
      //else {
      //  if seen => getlastsub with this element
      //  else recurse on tail
      //}
      toParse match {
        case Seq() => getLastSub(search, acc)
        case head +: tail => {
          // val head = toParse.head
          if (seen.contains(head._1)) getLastSub(search, acc :+ head) else {
            doRec(acc :+ head, seen + head._1, tail)
          }
        }
      }
    }

    val r = s.zipWithIndex
    doRec(List.empty[Tup], Set.empty[Char], r.drop(search._2 + 1))
  }

  def doOther(s: String, list: Seq[UniqueThing] = List.empty): String = {

    val all = (findAllMissing(s, list) ++ list)
    // println(s)
    all.filter(_.works) match {
      case Seq() => s.split("_")(0)
      case x => {
        val rem = x.sortBy(_.start).maxBy(_.length)
        doOther(replace(s, rem), all.map(_.drop(rem)).flatten)
      }
    }
  }

  def replace(s: String, idk: UniqueThing) = {
    //assuming they're already sorted
    // val idk = x.maxBy(_.length)
    // idk.print
    drop(s, idk.start, idk.end) + idk.letter
  }
}

class UniqueThing(val letter: Char, val start: Int, val end: Int, val upTo: Int, val works: Boolean) {
  lazy val length = if (works) end - start else -1

  def print = {
    println("Letter: " + letter + " " + start + " - " + end)
  }

  def drop(implicit dropped: UniqueThing) = {
    if (letter == dropped.letter || !isOutside) {
      None
    } else {
      Some(new UniqueThing(letter, start - shiftStart, end - shiftEnd, upTo - shiftEnd, works))
    }
  }

  def isOutside(implicit dropped: UniqueThing) = {
    dropped.end < start || dropped.start > upTo
  }

  def shiftStart(implicit d: UniqueThing) = {
    if (d.end < start) {
      2
    } else if (d.start < start) {
      1
    } else {
      0
    }
  }

  def shiftEnd(implicit dropped: UniqueThing) = {
    if (dropped.end < end) {
      2
    } else if (dropped.start < end) {
      1
    } else {
      0
    }
  }
}

class Holder(s: String) {
  def getLastSub(searched: Tup, s: Seq[Tup]) = {
    lazy val end = s match {
      case Seq() => searched._2
      case _ => s.last._2
    }

    s.lastIndexWhere(_._1 == searched._1) match {
      case -1 => new UniqueThing(searched._1, searched._2, end, end, false)
      case x => new UniqueThing(searched._1, searched._2, s(x)._2, s.last._2, true)
    }
  }

  //as a 'shortcut' only recompute letters where index < removed.end || letter == removed.letter
  //if > than end then just sub start and end by 2
  //define fileter method,
  //for more a shortcut  keep a reference of where the duplicate happened
  //if duplicate not between end, then keep?
  // def filter(r: Option[UniqueThing])(tup: Tup) = {
  //   r match {
  //     case Some(x) => tup._2 <= x.end -2 || tup._1 == x.letter
  //     case None => true
  //   }
  // }

  // def filter(l: Seq[UniqueThing])(tup: Tup) = {
  //   l.view.filter
  // }

  def findAllMissing(s: String, l: Seq[UniqueThing]) = {
    val f = findLongest(s)_
    // val filt = filter(l)_
    val filt = l.map(_.start)
    s.zipWithIndex.withFilter(x => !filt.contains(x._2)).map(f(_))
  }

  def findLongest(s: String)(search: Tuple2[Char, Int]) = {
    def doRec(acc: Seq[Tup], seen: Set[Char], toParse: Seq[Tup]): UniqueThing = {
      //logic should be:
      //if no more to parse, get last sub
      //else {
      //  if seen => getlastsub with this element
      //  else recurse on tail
      //}
      toParse match {
        case Seq() => getLastSub(search, acc)
        case head +: tail => {
          // val head = toParse.head
          if (seen.contains(head._1)) getLastSub(search, acc :+ head) else {
            doRec(acc :+ head, seen + head._1, tail)
          }
        }
      }
    }

    val r = s.zipWithIndex
    doRec(List.empty[Tup], Set.empty[Char], r.drop(search._2 + 1))
  }
}

object Util {

  def drop(s: String, indicies: Int*): String = {
    s.zipWithIndex.map(x => x._2 match {
      case y if (indicies contains y) => None
      case _ => Some(x._1)
      }).flatten.mkString("")
  }

}
