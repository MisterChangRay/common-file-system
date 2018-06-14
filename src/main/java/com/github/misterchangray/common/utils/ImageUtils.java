package com.github.misterchangray.common.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.Arrays;

/**
 * @author Created by rui.zhang on 2018/6/14.
 * @author rui.zhang
 * @version ver1.0
 * @email misterchangray@hotmail.com
 * @description
 */
@Component
public class ImageUtils {
    private Logger log = LoggerFactory.getLogger(getClass());
    public static String DEFAULT_PREVFIX = "thumb_";
    public static Boolean DEFAULT_FORCE = false;//建议该值为false


    public static void main(String[] args) {
        new ImageUtils().thumbnailImage("D:\\upload\\2018-06-14\\88d5c47f6b234ba390019501f2f6d9c9.jpg", 100, 150,DEFAULT_PREVFIX,DEFAULT_FORCE);
    }

    /**
     * 依据图片路径生成缩略图
     * @param imgData 图片数据
     * @param path 保存路径及文件名
     * @param w 缩略图宽
     * @param h 缩略图高
     * @param force  是否强制依照宽高生成缩略图(假设为false，则生成最佳比例缩略图)
     */
    public void thumbnailImageForBuffer(byte[] imgData, String path, int w, int h, Boolean force){
        if(null == force) force = DEFAULT_FORCE;
        if("".equals(FileUtils.getSuffix(path))) {
            log.debug("获取不到文件后缀, path:{}.",path);
            return;
        }
        try {
            ByteArrayInputStream in = new ByteArrayInputStream(imgData);
            log.debug("target image's size, width:{}, height:{}.",w,h);
            Image img = ImageIO.read(in);
            if(!force){
                // 依据原图与要求的缩略图比例，找到最合适的缩略图比例
                int width = img.getWidth(null);
                int height = img.getHeight(null);
                if((width*1.0)/w < (height*1.0)/h){
                    if(width > w){
                        h = Integer.parseInt(new java.text.DecimalFormat("0").format(height * w/(width*1.0)));
                        log.debug("change image's height, width:{}, height:{}.",w,h);
                    }
                } else {
                    if(height > h){
                        w = Integer.parseInt(new java.text.DecimalFormat("0").format(width * h/(height*1.0)));
                        log.debug("change image's width, width:{}, height:{}.",w,h);
                    }
                }
            }
            BufferedImage bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
            Graphics g = bi.getGraphics();
            g.drawImage(img, 0, 0, w, h, Color.LIGHT_GRAY, null);
            g.dispose();
            String p = path;
            File file = new File(path);
            if(!file.getParentFile().exists()) file.getParentFile().mkdirs();
            // 将图片保存在原文件夹并加上前缀
            ImageIO.write(bi, FileUtils.getSuffix(path).substring(1), file);
            log.debug("缩略图在原路径下生成成功");
        } catch (IOException e) {
            log.error("generate thumbnail image failed.",e);
        }

    }


    /**
     * <p>Title: thumbnailImage</p>
     * <p>Description: 依据图片路径生成缩略图 </p>
     * @param imagePath    原图片路径
     * @param w            缩略图宽
     * @param h            缩略图高
     * @param prevfix    生成缩略图的前缀
     * @param force        是否强制依照宽高生成缩略图(假设为false，则生成最佳比例缩略图)
     */
    public void thumbnailImage(String imagePath, int w, int h, String prevfix, Boolean force){
        if(null == prevfix) prevfix = DEFAULT_PREVFIX;
        if(null == force) force = DEFAULT_FORCE;
        File imgFile = new File(imagePath);
        if(imgFile.exists()){
            try {
                // ImageIO 支持的图片类型 : [BMP, bmp, jpg, JPG, wbmp, jpeg, png, PNG, JPEG, WBMP, GIF, gif]
                String types = Arrays.toString(ImageIO.getReaderFormatNames());
                String suffix = null;
                // 获取图片后缀
                if(imgFile.getName().indexOf(".") > -1) {
                    suffix = imgFile.getName().substring(imgFile.getName().lastIndexOf(".") + 1);
                }// 类型和图片后缀所有小写，然后推断后缀是否合法
                if(suffix == null || types.toLowerCase().indexOf(suffix.toLowerCase()) < 0){
                    log.error("Sorry, the image suffix is illegal. the standard image suffix is {}." + types);
                    return ;
                }
                log.debug("target image's size, width:{}, height:{}.",w,h);
                Image img = ImageIO.read(imgFile);
                if(!force){
                    // 依据原图与要求的缩略图比例，找到最合适的缩略图比例
                    int width = img.getWidth(null);
                    int height = img.getHeight(null);
                    if((width*1.0)/w < (height*1.0)/h){
                        if(width > w){
                            h = Integer.parseInt(new java.text.DecimalFormat("0").format(height * w/(width*1.0)));
                            log.debug("change image's height, width:{}, height:{}.",w,h);
                        }
                    } else {
                        if(height > h){
                            w = Integer.parseInt(new java.text.DecimalFormat("0").format(width * h/(height*1.0)));
                            log.debug("change image's width, width:{}, height:{}.",w,h);
                        }
                    }
                }
                BufferedImage bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
                Graphics g = bi.getGraphics();
                g.drawImage(img, 0, 0, w, h, Color.LIGHT_GRAY, null);
                g.dispose();
                String p = imgFile.getPath();
                // 将图片保存在原文件夹并加上前缀
                ImageIO.write(bi, suffix, new File(p.substring(0,p.lastIndexOf(File.separator)) + File.separator + prevfix +imgFile.getName()));
                log.debug("缩略图在原路径下生成成功");
            } catch (IOException e) {
                log.error("generate thumbnail image failed.",e);
            }
        }else{
            log.warn("the image is not exist.");
        }
    }


}