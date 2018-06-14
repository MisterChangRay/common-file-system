package com.github.misterchangray.common.utils;

/**
 * @author Created by rui.zhang on 2018/6/14.
 * @author rui.zhang
 * @version ver1.0
 * @email misterchangray@hotmail.com
 * @description
 */
public class FileUtils {
    /**
     * 获取文件后缀
     *
     * @return
     */
    public static String getSuffix(String filename) {
        if (null == filename) return "";
        return filename.replaceAll("^.+(?=\\.)", "");
    }
}
