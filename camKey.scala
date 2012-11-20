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
            sys.exit(-1)
            None 
        }
        
    val cam = getCamera(0).get
    
    def captureFrame(pixels:Array[Array[Int]]) = {
        Option(cam.grab) foreach { img =>
            val imgData = img.getBufferedImage.getData.getDataBuffer.asInstanceOf[java.awt.image.DataBufferByte]
            for(i <- 0 until width*height) 
                pixels(i%width)(i/width) = (
                    imgData.getElem(i*3+0).toInt +
                    imgData.getElem(i*3+1).toInt +
                    imgData.getElem(i*3+2).toInt
                )
        }
    }

    val robot = new Robot
    var avgtime = 0d
    var ex, ey = 0
    var pic1 = Array.ofDim[Int](width, height)
    var pic2 = Array.ofDim[Int](width, height)
    captureFrame(pic1)
    captureFrame(pic2)
    var tick = now
    while(true) {
        var gradL1, gradR1, gradL2, gradR2, gradL3, gradR3, 
            gradDL, gradDR, gradUL, gradUR,
            gradU1, gradD1, gradU2, gradD2, gradU3, gradD3, gradC = 0
        val edge = 3
        for(i <- edge until width-edge; j <- edge until height-edge) {
            gradC  += abs(pic1(i)(j) - pic2(i)(j))
            gradL1 += abs(pic1(i)(j) - pic2(i-1)(j))
            gradL2 += abs(pic1(i)(j) - pic2(i-2)(j))
            gradL3 += abs(pic1(i)(j) - pic2(i-3)(j))
            gradR1 += abs(pic1(i)(j) - pic2(i+1)(j))
            gradR2 += abs(pic1(i)(j) - pic2(i+2)(j))
            gradR3 += abs(pic1(i)(j) - pic2(i+3)(j))
            gradUL += abs(pic1(i)(j) - pic2(i-1)(j-1))
            gradUR += abs(pic1(i)(j) - pic2(i+1)(j-1))
            gradDL += abs(pic1(i)(j) - pic2(i-1)(j+1))
            gradDR += abs(pic1(i)(j) - pic2(i+1)(j+1))
            gradU1 += abs(pic1(i)(j) - pic2(i)(j-1))
            gradU2 += abs(pic1(i)(j) - pic2(i)(j-2))
            gradU3 += abs(pic1(i)(j) - pic2(i)(j-3))
            gradD1 += abs(pic1(i)(j) - pic2(i)(j+1))
            gradD2 += abs(pic1(i)(j) - pic2(i)(j+2))
            gradD3 += abs(pic1(i)(j) - pic2(i)(j+3))
        }
        var gradsx = List(gradL1, gradR1, gradL2, gradR2, gradL3, gradR3, gradUL, gradUR, gradDL, gradDR, gradC).zipWithIndex.minBy(_._1)._2
        var x = -(gradsx match {
            case 0 => -1
            case 1 => +1
            case 2 => -2
            case 3 => +2
            case 4 => -3
            case 5 => +3
            case 6 => -2
            case 7 => +2
            case 8 => -2
            case 9 => +2
            case 10=> 0
        })
        var gradsy = List(gradU1, gradD1, gradU2, gradD2, gradU3, gradD3, gradUL, gradUR, gradDL, gradDR, gradC).zipWithIndex.minBy(_._1)._2
        var y = gradsy match {
            case 0 => -1
            case 1 => +1
            case 2 => -2
            case 3 => +2
            case 4 => -3
            case 5 => +3
            case 6 => -2
            case 7 => -2
            case 8 => +2
            case 9 => +2
            case 10=> 0
        }
        
        val p = MouseInfo.getPointerInfo.getLocation
        if(x >= 2 || (ex >= 1 && x >= 1)) {
            robot.keyPress(KeyEvent.VK_RIGHT)
            robot.mouseMove(p.x+x*3, p.y)
        } else if(x <= -2 || (ex <= -1 && x <= -1)) {
            robot.keyPress(KeyEvent.VK_LEFT)
            robot.mouseMove(p.x+x*3, p.y)
        } else {
            robot.keyRelease(KeyEvent.VK_RIGHT)
            robot.keyRelease(KeyEvent.VK_LEFT)
        }

        ex = x
        ey = y
        val swap = pic1
        pic1 = pic2
        pic2 = swap
        captureFrame(pic2)
        
        print((x,y) + "                 \t\t\t " + ("%1.2f" format avgtime) + "           \r")
        avgtime = (since(tick)*0.2d + avgtime*0.8d)
        tick = now
    }
    
    // cleanup
    cam.stop
}
