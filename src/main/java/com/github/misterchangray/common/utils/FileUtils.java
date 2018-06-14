package com.github.misterchangray.common.utils;

import javax.imageio.ImageIO;
import java.util.Arrays;

/**
 * @author Created by rui.zhang on 2018/6/14.
 * @author rui.zhang
 * @version ver1.0
 * @email misterchangray@hotmail.com
 * @description
 */
public class FileUtils {
    public static void main(String[] args) {
        System.out.println(getSuffix("jpg").toLowerCase().substring(1));
    }

    /**
     * 文件是否为图片;通过后缀判断
     *
     * @return
     */
    public static boolean isImg(String filepath) {
        if (null == filepath) return false;
        String suffix = getSuffix(filepath);
        if("".equals(suffix)) return false;
        return  Arrays.toString(ImageIO.getReaderFormatNames()).toLowerCase().contains(suffix.substring(1).toLowerCase());
    }


    /**
     * 文件名前增加前缀
     *
     * @return
     */
    public static String addPrefix(String filepath, String prefix) {
        if (null == filepath) return "";
        return filepath.replaceAll("^(.+[/\\\\])",  "$1" + prefix);
    }

    /**
     * 获取文件后缀
     *
     * @return
     */
    public static String getSuffix(String filename) {
        if (null == filename || !filename.contains(".")) return "";
        return filename.replaceAll("^.+(?=\\.)", "");
    }
}
