package com.github.misterchangray.service.file.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 *
 * 在打包文件时,描述文件
 * @author Created by rui.zhang on 2018/6/11.
 * @author rui.zhang
 * @version ver1.0
 * @email misterchangray@hotmail.com
 * @description
 */
@ApiModel(value = "com.github.misterchangray.service.file.dto.FileInfo")
public class FileInfo {
    /**
     * 指定待操作文件的ID
     */
    @ApiModelProperty(value="文件ID")
    private String fileId;
    /**
     * 指定如何操作文件
     *
     * {target:"/身份证/身份证证明.jpg"};此参数将会执行以下操作：
     * 1.将文件移动到 "/身份证/" 目录下(如没有则创建)
     * 2.将移动后的文件名称为 "身份证证明.jpg"
     */
    @ApiModelProperty(value="文件欲打包路径和名称;这里可以指定文件打包到那个路径;以及重命名文件名称")
    private String target;

    public String getFileId() {
        return fileId;
    }

    public void setFileId(String fileId) {
        this.fileId = fileId;
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }
}
