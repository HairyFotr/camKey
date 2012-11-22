import com.googlecode.javacv._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_highgui._
import util.Random._
import Thread.sleep
import math._
import collection.mutable.{HashMap,HashSet,ListBuffer,LinkedHashMap}
import java.awt.{MouseInfo,Robot}
import java.awt.event.{KeyEvent,InputEvent}
import scala.concurrent._
import scala.concurrent.util._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object camMouse extends App {
    var timeDivisor = 1000000L
    def since(time: Int): Int = now-time
    def now = (System.nanoTime()/timeDivisor).toInt
    def time(func: => Unit) = {
        val startTime = now
        func
        now-startTime
    }
    def pad(i: Int, p: Int = 4) = "0"*(p-i.toString.size)+i.toString
    val (inf,ninf) = (Double.PositiveInfinity, Double.NegativeInfinity)
    var width = 640
    var height = 480
    
    var switches = args.dropWhile(_ matches "[0-9]*").toSet

    // init
    def getCamera(i:Int):Option[FrameGrabber] = 
        try { 
            val cam = new OpenCVFrameGrabber(i) 
            cam.setImageWidth(width)
            cam.setImageHeight(height)
            cam.start
            Some(cam)
        } catch { 
            case e:Throwable => 
            e.printStackTrace
            println()
            println("Is the camera on... did you follow the README?")
            sys.exit(-1)
            None 
        }
        
    val camID = try { args(0).toInt } catch { case e:Exception => 0 }
    val cam = getCamera(camID).get
    
    val imgRange = (0 until width*height)
    def captureFrame(pixels:Array[Array[Int]]) = {
        Option(cam.grab) foreach { img =>
            val imgData = img.getBufferedImage.getData.getDataBuffer.asInstanceOf[java.awt.image.DataBufferByte]
            for(i <- imgRange) 
                pixels(i%width)(i/width) = (
                    imgData.getElem(i*3+0).toInt +
                    imgData.getElem(i*3+1).toInt +
                    imgData.getElem(i*3+2).toInt
                )
        }
    }

    var robot = new Robot
    var avgTime = 0d
    var pic1 = Array.ofDim[Int](width, height)
    var pic2 = Array.ofDim[Int](width, height)
    captureFrame(pic1)
    captureFrame(pic2)
    var tick = now
    val edge = 25
    var ex, ey = 0d
    val randPoints = Array.tabulate(77)(a=> (nextInt(width-(edge+1)*2)+edge+1, nextInt(height-(edge+1)*2)+edge+1)).distinct.par
    var cnt = 0
    var x0,y0 = 0d
    var xt1,xt2,yt1,yt2,xt,yt = 0d
    var xcnt1,xcnt2 = 0
    var ecnt1,ecnt2 = 0
    var cntx0,cnty0 = 0
    val init = 30
    var noise = List[(Double,Double)]()
    var futurepic = future {}
    val Arange = (0 until 7).toArray
    println("Be still for a few seconds :)")
    var clickTime = now
    while(true) {
        Await.result(futurepic, Duration.Inf)
        var vectors = for((x,y) <- randPoints) yield {
            def getArea1(x:Int,y:Int,pic:Array[Array[Int]]) = Array[Int](
                 pic(x)(y)*2,
                 pic(x+1)(y),
                 pic(x-1)(y),
                 pic(x)(y+1),
                 pic(x)(y-1),
                 pic(x+1)(y+1)/2,
                 pic(x-1)(y+1)/2,
                 pic(x+1)(y-1)/2,
                 pic(x-1)(y-1)/2
            )
            val exP = getArea1(x,y, pic1)
            def getArea(x:Int,y:Int,pic:Array[Array[Int]]) = {
                val cP = getArea1(x,y,pic)
                Arange.foldLeft(0)((score, i) => score + abs(exP(i)-cP(i)))
            }
                
            var minDist = inf
            var minP = (0d,0d)
            for(i <- -edge until edge; j <- -edge until edge) {
                val currDist = getArea(x+i,y+j, pic2) + sqrt(math.pow(i, 2) + math.pow(j, 2))*7
                if(currDist < minDist) {
                    minDist = currDist
                    minP = (i,j)
                }
            }
            //if(minP != (0,0)) 
            minP
        }
        futurepic = future {
            val swap = pic1
            pic1 = pic2
            pic2 = swap
            captureFrame(pic2)
        }
        
        val (x,y) = {
            var out = vectors.filterNot(_==(0d,0d)).foldLeft((0d,0d))((acc, v) => (acc._1+v._1, acc._2+v._2))
            out = (out._1/vectors.size*avgTime*1.3, out._2/vectors.size*(avgTime*3))
            if(cnt <= init) 
                (-out._1, out._2)
            else
                (-out._1-x0, out._2-y0)
        }
        
        
        ex = (ex + x)/2
        ey = (ey + y)/2
        val cnt1 = vectors.map(_._1).count(_ < xt1)-xcnt1
        val cnt2 = vectors.map(_._1).count(_ > xt2)-xcnt2
        
        if(cnt < init) {
            noise = noise :+ (x,y)
        } else if(cnt == init) {
            def noiseAvg(m:((Double,Double))=>Double)(f:Double=>Boolean) = { 
                val n = noise.map(m).filter(f)
                n.sum/n.size
            }
            
            x0 = noiseAvg(_._1)(a=>true)
            y0 = noiseAvg(_._2)(a=>true)
            xt1 = (noiseAvg(_._1)(_ < x0)-x0)
            xt2 = (noiseAvg(_._1)(_ > x0)-x0)
            yt1 = (noiseAvg(_._2)(_ < y0)-y0)
            yt2 = (noiseAvg(_._2)(_ > y0)-y0)
            xt = min(abs(xt1),abs(xt2))
            yt = min(abs(yt1),abs(yt2))
            
            println(List(x0,y0,xt1,xt2,yt1,yt2).map(_.toInt).mkString(","))
            //println("A bit more... :)")
        } else if(cnt < init*2 && (switches contains "click")) {
            xcnt1 += vectors.map(_._1).count(_ < xt1)
            xcnt2 += vectors.map(_._1).count(_ > xt2)
        } else if(cnt == init*2 && (switches contains "click")) {
            xcnt1 /= init
            xcnt2 /= init
        } else if(cnt == init*2+1) {
            println("Move now :)")
        } else {

            val p = MouseInfo.getPointerInfo.getLocation
            val (vx,vy) = (
                if(ex <= xt1/2 || ex >= xt2/2) (ex - signum(ex)*xt).toInt else 0,
                if(ey <= yt1/2 || ey >= yt2/2) (ey - signum(ey)*yt).toInt else 0
                //ex.toInt,ey.toInt
            )

            if(switches contains "click") {
            if(cnt1 >= 1 && cnt2 >= 1 && ecnt1 >= 1 && ecnt2 >= 1 && ecnt1+ecnt2 >= 3 && vy > 0 && since(clickTime) > 700) {
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
                cntx0 = p.x
                cnty0 = p.y
            }
            }

            if(since(clickTime) > 400) {
                robot.mouseMove(p.x+vx, p.y+vy)
            }
        }
        ecnt1 = cnt1
        ecnt2 = cnt2

        cnt += 1
        print("\r" + (pad(x.toInt),pad(y.toInt)) + (pad(cnt1.toInt),pad(cnt2.toInt)) + "                 \t\t\t " + ("%1.2f" format avgTime) + "           \r")
        avgTime = (since(tick)*0.2d + avgTime*0.8d)
        tick = now
    }
    
    // cleanup
    cam.stop
}
