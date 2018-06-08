package com.github.misterchangray.controller;

import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.common.annotation.Authentication;
import com.github.misterchangray.service.file.FileService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

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
    private FileService fileService;



    @ApiOperation(value = "文件上传接口", notes = "文件上传接口")
    @ApiImplicitParams({
            @ApiImplicitParam(name="pid", value = "父ID", required = false, paramType = "query", dataType = "string"),
            @ApiImplicitParam(name="shortcut", value = "简称", required = false, paramType = "query", dataType = "string"),
    })
    @Authentication()
    @RequestMapping(value = "/upload", method = RequestMethod.POST)
    @ResponseBody
    public ResultSet fileUpload() {
        ResultSet res = ResultSet.build();


        res.setData(null);
        return res;
    }



}
