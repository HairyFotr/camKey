import com.googlecode.javacv._
import com.googlecode.javacv.cpp.opencv_core._
import com.googlecode.javacv.cpp.opencv_highgui._
import Thread.sleep
import math._
import collection.mutable.{HashMap,HashSet,ListBuffer,LinkedHashMap}
import java.awt.{MouseInfo,Robot}
import java.awt.event.KeyEvent

object camKey extends App {
    var timeDivisor = 1000000L
    def since(time: Int): Int = now-time
    def now = (System.nanoTime()/timeDivisor).toInt
    def time(func: => Unit) = {
        val startTime = now
        func
        now-startTime
    }
    val (inf,ninf) = (Double.PositiveInfinity, Double.NegativeInfinity)
    var width = 640
    var height = 480

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
        
    val cam = getCamera(try { args(0).toInt } catch { case e:Exception => 0 }).get
    
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

    val robot = new Robot
    var avgtime = 0d
    var ex = 0
    var pic1 = Array.ofDim[Int](width, height)
    var pic2 = Array.ofDim[Int](width, height)
    captureFrame(pic1)
    captureFrame(pic2)
    var tick = now
    while(true) {
        val edge = 17
        
        var gradL1, gradR1, gradL2, gradR2, gradL3, gradR3, gradL4, gradR4, gradL5, gradR5, gradL6, gradR6, gradL7, gradR7,
            gradDL, gradDR, gradUL, gradUR, gradDL2, gradDR2, gradUL2, gradUR2,
            gradC = 0
        for(i <- edge until width-edge; j <- edge until height-edge) {
            gradL1 += abs(pic1(i)(j) - pic2(i-1)(j))
            gradL2 += abs(pic1(i)(j) - pic2(i-2)(j))
            gradL3 += abs(pic1(i)(j) - pic2(i-3)(j))
            gradL4 += abs(pic1(i)(j) - pic2(i-4)(j))
            gradL5 += abs(pic1(i)(j) - pic2(i-7)(j))
            gradL6 += abs(pic1(i)(j) - pic2(i-11)(j))
            gradL7 += abs(pic1(i)(j) - pic2(i-17)(j))
            gradC  += abs(pic1(i)(j) - pic2(i)(j))
            gradR1 += abs(pic1(i)(j) - pic2(i+1)(j))
            gradR2 += abs(pic1(i)(j) - pic2(i+2)(j))
            gradR3 += abs(pic1(i)(j) - pic2(i+3)(j))
            gradR4 += abs(pic1(i)(j) - pic2(i+4)(j))
            gradR5 += abs(pic1(i)(j) - pic2(i+7)(j))
            gradR6 += abs(pic1(i)(j) - pic2(i+11)(j))
            gradR7 += abs(pic1(i)(j) - pic2(i+17)(j))
            gradUL += abs(pic1(i)(j) - pic2(i-2)(j-1))
            gradUR += abs(pic1(i)(j) - pic2(i+2)(j-1))
            gradDL += abs(pic1(i)(j) - pic2(i-2)(j+1))
            gradDR += abs(pic1(i)(j) - pic2(i+2)(j+1))
            gradUL2 += abs(pic1(i)(j) - pic2(i-5)(j-2))
            gradUR2 += abs(pic1(i)(j) - pic2(i+5)(j-2))
            gradDL2 += abs(pic1(i)(j) - pic2(i-5)(j+2))
            gradDR2 += abs(pic1(i)(j) - pic2(i+5)(j+2))
        }
        var gradsx = List(gradC, gradUL, gradUR, gradDL, gradDR, 
            gradL1, gradR1, gradL2, gradR2, gradL3, gradR3, gradL4, gradR4, 
            gradL5, gradR5, gradL6, gradR6, gradL7, gradR7,
            gradUL, gradUR, gradDL, gradDR
        ).zipWithIndex.minBy(_._1)._2
        
        var x = -(gradsx match {
            case 0 =>  0
            case 1 => -3
            case 2 => +3
            case 3 => -3
            case 4 => +3
            case 5 => -1
            case 6 => +1
            case 7 => -2
            case 8 => +2
            case 9 => -3
            case 10=> +3
            case 11=> -4
            case 12=> +4
            case 13=> -7
            case 14=> +7
            case 15=> -11
            case 16=> +11
            case 17=> -18
            case 18=> +18
            case 19=> -6
            case 20=> +6
            case 21=> -6
            case 22=> +6
        })        
        
        val p = MouseInfo.getPointerInfo.getLocation
        if(x >= 6 || (ex >= 4 && x >= 2) || (ex >= 3 && x >= 1)) {
            robot.keyPress(KeyEvent.VK_RIGHT)
            //robot.mouseMove(p.x+x*5, p.y)
        } else if(x <= -6 || (ex <= -4 && x <= -2) || (ex <= -3 && x <= -1)) {
            robot.keyPress(KeyEvent.VK_LEFT)
            //robot.mouseMove(p.x+x*5, p.y)
        } else {
            robot.keyRelease(KeyEvent.VK_RIGHT)
            robot.keyRelease(KeyEvent.VK_LEFT)
        }

        ex = (ex + x)/2

        val swap = pic1
        pic1 = pic2
        pic2 = swap
        captureFrame(pic2)
        
        print("\r" + x + "                 \t\t\t " + ("%1.2f" format avgtime) + "           \r")
        avgtime = (since(tick)*0.2d + avgtime*0.8d)
        tick = now
    }
    
    // cleanup
    cam.stop
}
