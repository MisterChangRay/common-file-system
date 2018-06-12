package com.github.misterchangray.controller;

import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.common.annotation.Authentication;
import com.github.misterchangray.common.enums.ResultEnum;
import com.github.misterchangray.dao.entity.CommonFile;
import com.github.misterchangray.service.file.FileService;
import com.github.misterchangray.service.file.dto.FileInfo;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpSession;
import java.util.List;

/**
 * 文件控制器
 *
 * @author Rui.Zhang/misterchangray@hotmail.com
 * @author Created on  3/23/2018.
 */
@Api(tags ="文件管理", description = "FileSysController")
@Controller
@RequestMapping("/v1/file")
public class FileSysController {
    @Autowired
    private HttpSession httpSession;
    @Autowired
    private FileService fileService;

    @Value("${upload.max.size:10485760}")
    private Long maxSize;




    @ApiOperation(value = "文件上传接口", notes = "文件上传接口")
    @ApiImplicitParams({
            @ApiImplicitParam(name="file", value = "欲上传的文件", required = true, paramType = "query", dataType = "string")
    })
    @Authentication()
    @RequestMapping(value = "/uploadFile", method = RequestMethod.POST)
    @ResponseBody
    public ResultSet<CommonFile> uploadFile(@RequestParam("file") MultipartFile uploadFile,
                                            @RequestParam("appKey") String appKey) throws Exception{
        if(null == uploadFile) return ResultSet.build(ResultEnum.INVALID_REQUEST);
        if(maxSize < uploadFile.getSize()) return ResultSet.build(ResultEnum.INVALID_REQUEST).setMsg("文件不能超过10MB");

        return fileService.saveFile(uploadFile, appKey);
    }


    /**
     * 文件打包接口;
     * 被打包的文件将会经过以下操作:
     * 1.寻找被打包文件ID
     * 2.如果target 指定了目录,则移动到target目录下;
     * 3.如果target 指定了文件名和后缀;则修改文件名和后缀
     * 4.完成所有的文件后打包文件夹
     * 5.根据 zipName 将打包后的压缩文件重命名
     * 例如
     * 传入参数为 {fileInfos:[{fileId:"1", target:target:"/身份证/身份证证明.jpg"}],zipName:"ceshi.zip", appKey:"185gs158qje" };将会执行以下操作：
     * 1.移动文件ID为1的文件到 "/身份证/" 目录下(如没有此目录则创建)
     * 2.将移动后的文件名称为 "身份证证明.jpg"
     * 3.连同目录打包文件并将打包后的zip文件命名为 "ceshi.zip"
     * tips:
     * - 若 target 只有目录;则只移动文件不改文件名
     * - 若 target 只有文件名;则平级打包并修改文件名
     * - 若 target 为null;则直接进行平级打包
     * @param fileInfos
     * @param zipName
     * @param appKey
     * @return
     * @throws Exception
     */
    @ApiOperation(value = "文件打包接口", notes = "文件打包接口")
    @ApiImplicitParams({
            @ApiImplicitParam(name="fileInfos", value = "欲打包的文件信息;此字段传入JSON串;包含以下字段：<br>fileId 文件ID<br>target 文件处理信息<br>", required = true, paramType = "query", dataType = "string"),
            @ApiImplicitParam(name="zipName", value = "打包文件名;可以为空", required = true, paramType = "query", dataType = "string"),
            @ApiImplicitParam(name="appKey", value = "分配的appKey", required = false, paramType = "query", dataType = "string"),
    })
    @Authentication()
    @RequestMapping(value = "/packFilesToZip", method = RequestMethod.POST)
    @ResponseBody
    public ResultSet<CommonFile> packFilesToZip(@RequestParam("fileInfos") List<FileInfo> fileInfos,
                                                @RequestParam(value = "zipName", required = false) String zipName,
                                                @RequestParam("appKey") String appKey) throws Exception{
        if(null == fileInfos) return ResultSet.build(ResultEnum.INVALID_REQUEST);
        if(null == appKey) return ResultSet.build(ResultEnum.INVALID_REQUEST);

        return fileService.packFilesToZip(fileInfos, zipName, appKey);
    }

}
