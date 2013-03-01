import util.Random._
import Thread.sleep
import math._
import collection.mutable.{HashMap,HashSet,ListBuffer,LinkedHashMap}
import collection.parallel.mutable.ParArray
import java.awt.{MouseInfo,Robot}
import java.awt.event.{KeyEvent,InputEvent}
import scala.concurrent._
import scala.concurrent.util._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import System.err

object Utils {
  var timeDivisor = 1000000L
  def since(time: Int): Int = now-time
  def now: Int = (System.nanoTime()/timeDivisor).toInt
  def time(func: => Unit): Int = {
    val startTime = now
    func
    now-startTime
  }

  def pad(i: Int, p: Int = 4): String = "0"*(p-i.toString.size)+i.toString
  val (inf,ninf) = (Double.PositiveInfinity, Double.NegativeInfinity)

  def withDefault[T](func: => T, default: => T ): T = try { func } catch { case _: Throwable => default}
  def withExit[T](func: => T, exit: => Any = { }): T = try { func } catch { case _: Throwable => exit; sys.exit(-1) }
 
  import com.googlecode.javacv._
  import com.googlecode.javacv.cpp.opencv_core._
  import com.googlecode.javacv.cpp.opencv_highgui._
  object Camera {
    val FrameGrabbers = HashMap[Int, OpenCVFrameGrabber]()
    
    def getCamera(camIds: List[Int], width: Int = 640, height: Int = 480): Option[Camera] = {
      for(camId <- camIds) {
        val out = Camera(camId, width, height)
        if(out.isStarted) return Some(out)
      }
      
      None
    }
    
    def getFrameGrabber(camId: Int = 0, width: Int = 640, height: Int = 480): Option[OpenCVFrameGrabber] = {
      try {
        val cam = new OpenCVFrameGrabber(camId) 
        cam.setImageWidth(width)
        cam.setImageHeight(width)
        cam.start
        cam.grab
        FrameGrabbers += camId -> cam
        Some(cam)
      } catch {
        case e: Exception => 
        err.println("Failed to initialize camera "+camId+" @ "+width+"x"+height)
        None
      }
    }
    def apply(camId: Int = 0, width: Int = 640, height: Int = 480): Camera = {
      new Camera(camId, width, height)
    }
  }
  class Camera(val camId: Int = 0, val width: Int = 640, val height: Int = 480) {
    var camOpt: Option[OpenCVFrameGrabber] = Camera.getFrameGrabber(camId, width, height)
    def isStarted: Boolean = camOpt.isDefined
    lazy val cam = camOpt.get
    lazy val grabRange = (0 until width*height)
    
    def captureFrame(pixels: Array[Array[Int]]) {
      for(img <- Option(cam.grab)) {
        val imgData = img.getBufferedImage.getData.getDataBuffer.asInstanceOf[java.awt.image.DataBufferByte] //surely there's an easier way
        for(i <- grabRange) {
          pixels(i%width)(i/width) = (
            imgData.getElem(i*3 + 0).toInt +
            imgData.getElem(i*3 + 1).toInt +
            imgData.getElem(i*3 + 2).toInt)
        }
      }
    }
  }
}

object CamKey extends App {
  import Utils._
  
  // parse switches
  val (cam, modes, actions) = {
    val camIds = ListBuffer[Int]()
    val modes = HashSet[String]()
    val actions = HashSet[String]()
    var size = (640,480)
    
    if(args exists (_ matches "-?-?help")) {
      println("""|Usage: -c[CAMID] -m[MODE] -s[WIDTHxHEIGHT] [OPTIONS]
                 |
                 |Allows you to control your keyboard or mouse with your camera.
                 |
                 |Options info:
                 |  -mmouse           use mouse
                 |  -mkeyboard(x|y)   use keyboard, optionally only for x or y
                 |  -moutput          output to console
                 |  flip(x|y)         flip axes (useful if moving camera)
                 |  click             click gesture for mouse mode (WIP)
                 |
                 |Note: multiple modes can be used simultaneously""".stripMargin)
      sys.exit(0)
    }

    val legalModes = Set("mouse", "keyboard", "keyboardx", "keyboardy", "output")
    
    val camIdReg = "-c([0-9]*)".r
    val modeReg = "-m([0-9a-z]*)".r
    val sizeReg = "-s([0-9]*)x([0-9]*)".r
    args foreach {
      case camIdReg(camId) => 
        camIds += camId.toInt // additional indices used as fallback
      case modeReg(mode) => 
        modes += mode.toLowerCase
        if(!legalModes.contains(mode.toLowerCase)) err.println("Unknown mode: "+mode)
      case sizeReg(width,height) => 
        size = (width.toInt, height.toInt)
      case action =>
        actions += action
    }
    if(camIds.isEmpty) camIds += 0
    if(modes.isEmpty) modes += "mouse"

    val cam = withExit(
      Camera.getCamera(camIds.toList, size._1, size._2).get,
      err.println("Couldn't initialize any of the desired cameras."))
      
    err.println("Initialized cam "+cam.camId+" @ "+cam.width+"x"+cam.height)

    (cam, modes, actions)
  }

  var robot: Robot = new Robot
  var avgTime = 0d

  var pic1, pic2 = Array.ofDim[Int](cam.width, cam.height)
  cam.captureFrame(pic1)
  cam.captureFrame(pic2)
  var tick = now

  val edge = 20 // TODO: resolution independant
  var ex, ey = 0d
  var cnt = 0
  var x0, xt1,xt2,xt = 0d
  var y0, yt1,yt2,yt = 0d
  
  var xcnt1,xcnt2 = 0
  var ecnt1,ecnt2 = 0
  var cntx0,cnty0 = 0

  val noiseIters = 50
  var noise = List[(Double,Double)]()
  println("Be still for a few seconds :)")

  var futurePic = future {}
  var clickTime = now
  while(true) {
    val randPoints = ParArray.tabulate(200) { _ =>
      (nextInt(cam.width-(edge+1)*2)+(edge+1), 
      nextInt(cam.height-(edge+1)*2)+(edge+1))
    }

    def getPatch(x: Int, y: Int, pic: Array[Array[Int]]): Array[Int] = 
      Array(
        pic(x-1)(y-1)/2, pic(x)(y-1), pic(x+1)(y-1)/2,
        pic(x-1)(y),     pic(x)(y)*2, pic(x+1)(y),
        pic(x-1)(y+1)/2, pic(x)(y+1), pic(x+1)(y+1)/2)
        
    val patchSize = 9
    val patchRange = Array.range(0, patchSize)

    Await.result(futurePic, Duration.Inf)

    val vectors = randPoints map { case (x,y) =>
      val exPatch = getPatch(x,y, pic1)
      def comparePatches(patch: Array[Int]): Int = patchRange.foldLeft(0)((score, i) => score + abs(exPatch(i)-patch(i)))
          
      var minDist = inf
      var minP = (0d,0d)
      var i, j = -edge
      do {
        do {
          val currDist = comparePatches(getPatch(x+i,y+j, pic2)) + sqrt(math.pow(i, 2) + math.pow(j, 2))*10
          if(currDist < minDist) {
            minDist = currDist
            minP = (i,j)
          }
          i += 1
        } while(i <= edge)
        j += 1
        i = -edge
      } while(j <= edge)
      
      minP
    }
    
    futurePic = future {
      val swap = pic1
      pic1 = pic2
      pic2 = swap
      cam.captureFrame(pic2)
    }
    
    val (dx,dy) = {
      val vecs = vectors.filter(vec => abs(vec._1)+abs(vec._2) >= 2)
      val sum = vecs.foldLeft((0d,0d))((acc, vec) => (acc._1+vec._1, acc._2+vec._2))
      val avg = (sum._1/vecs.size, sum._2/vecs.size)
      
      val out = (avg._1*(avgTime*1.5), avg._2*(avgTime*2.5))
      
      if(cnt <= noiseIters) {
        (-out._1, out._2)
      } else {
        (-out._1-x0, out._2-y0)
      }
    }
    
    ex = (ex + dx)/2
    ey = (ey + dy)/2
    
    if(cnt < noiseIters) {
      noise = noise :+ (dx,dy)
    } else if(cnt == noiseIters) {
      def noiseAvg(m: ((Double, Double)) => Double)(f: Double => Boolean): Double = { 
          val n = noise.map(m).filter(f)
          n.sum/n.size
      }
      val noiseAvgx = noiseAvg(_._1) _
      val noiseAvgy = noiseAvg(_._2) _
      
      x0 = noiseAvgx(x => true)
      y0 = noiseAvgy(x => true)
      xt1 = noiseAvgx(_ < x0) - x0
      xt2 = noiseAvgx(_ > x0) - x0
      yt1 = noiseAvgy(_ < y0) - y0
      yt2 = noiseAvgy(_ > y0) - y0
      xt = min(abs(xt1), abs(xt2))
      yt = min(abs(yt1), abs(yt2))
      
      err.println(List(x0,y0,xt1,xt2,yt1,yt2).map(_.toInt).mkString(","))
      //err.println("A bit more... :)")
    } else if(cnt < noiseIters*2 && (actions contains "click")) {
      xcnt1 += vectors.map(_._1).count(_ < xt1)
      xcnt2 += vectors.map(_._1).count(_ > xt2)
    } else if(cnt == noiseIters*2 && (actions contains "click")) {
      xcnt1 /= noiseIters
      xcnt2 /= noiseIters
    } else if(cnt == noiseIters*2 + 1) {
      ex = 0
      ey = 0
      err.println("Move now :)")
    } else {
      if(since(clickTime) > 400) {
        def flipx(x: Int): Int = if((actions contains "flip") || (actions contains "flipx")) -x else x
        def flipy(y: Int): Int = if((actions contains "flip") || (actions contains "flipy")) -y else y

        modes foreach {
          case "mouse" =>
            val p = MouseInfo.getPointerInfo.getLocation
            val sensitivity = 0.8 // 1 should remove almost all noise, higher values reduce sensitivity
            var (vx,vy) = (
              flipx(if(ex < 0) -pow(ex/(xt1*sensitivity), 1.6).toInt else pow(ex/(xt2*sensitivity), 1.6).toInt),
              flipy(if(ey < 0) -pow(ey/(yt1*sensitivity), 2).toInt else pow(ey/(yt2*sensitivity), 2).toInt))
            
            if(actions contains "click") {
              val cnt1 = vectors.map(_._1).count(_ < xt1)-xcnt1
              val cnt2 = vectors.map(_._1).count(_ > xt2)-xcnt2
              if(cnt1 >= 1 && cnt2 >= 1 && ecnt1 >= 2 && ecnt2 >= 2 && cnt1+cnt2+ecnt1+ecnt2 >= 6 && abs(ecnt1-ecnt2) <= 5 && since(clickTime) > 700) {
                println
                robot.mouseMove(cntx0, cnty0)
                ex = 0
                ey = 0
                robot.mousePress(InputEvent.BUTTON1_MASK)
                sleep(10)
                robot.mouseRelease(InputEvent.BUTTON1_MASK)
                clickTime = now
                println
              } else if(cnt1+cnt2 <= 1) {
                val p = MouseInfo.getPointerInfo.getLocation
                cntx0 = p.x
                cnty0 = p.y
              }
              ecnt1 = cnt1
              ecnt2 = cnt2
            }
            robot.mouseMove(p.x+vx, p.y+vy)
            
          case str @ ("keyboard" | "keyboardx"  | "keyboardy") =>
            val sensitivity = 1.75 // 1 should remove almost all noise, higher values reduce sensitivity
            var (vx,vy) = (
              flipx(if(ex < 0) -pow(ex/(xt1*sensitivity), 1.6).toInt else pow(ex/(xt2*sensitivity), 1.6).toInt),
              flipy(if(ey < 0) -pow(ey/(yt1*sensitivity), 2).toInt else pow(ey/(yt2*sensitivity), 2).toInt))
              
            def key(cond: Boolean, key: Int) {
              if(cond) robot.keyPress(key) else robot.keyRelease(key)
            }
            
            val lim = 3
            if(!str.endsWith("y")) {
              key(vx > +lim, KeyEvent.VK_RIGHT)
              key(vx < -lim, KeyEvent.VK_LEFT)
            }
            if(!str.endsWith("x")) {
              key(vy > +lim, KeyEvent.VK_DOWN)
              key(vy < -lim, KeyEvent.VK_UP)
            }
            
          case "output" =>
            val sensitivity = 1.0 // 1 should remove almost all noise, higher values reduce sensitivity
            var (vx,vy) = (
              flipx(if(ex < 0) -pow(ex/(xt1*sensitivity), 1.6).toInt else pow(ex/(xt2*sensitivity), 1.6).toInt),
              flipy(if(ey < 0) -pow(ey/(yt1*sensitivity), 2).toInt else pow(ey/(yt2*sensitivity), 2).toInt))
              
            println(vx+" "+vy)
        }
      }
    }

    cnt += 1
    //err.print("\r" + (pad(x.toInt),pad(y.toInt)) + (pad(cnt1.toInt),pad(cnt2.toInt)) + "               \t\t\t " + ("%1.2f" format avgTime) + "         \r")
    err.print("\r" + ("%1.2f" format avgTime) + "             \r")
    avgTime = (since(tick)*0.2d + avgTime*0.8d)
    tick = now
  }
}
