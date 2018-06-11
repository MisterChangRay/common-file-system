package com.github.misterchangray.service.file;

import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.dao.entity.CommonFile;
import com.github.misterchangray.service.BaseService;
import com.github.misterchangray.service.file.dto.FileInfo;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;

/**
 * 文件服务
 *
 * @author Rui.Zhang/misterchangray@hotmail.com
 * @author Created on 3/20/2018.
 */
public interface FileService extends BaseService<CommonFile> {
    ResultSet<CommonFile> saveFile(MultipartFile uploadFile, String appKey) throws IOException;


    /**
     *
     * 文件打包接口;
     * 被打包的文件将会经过以下操作:
     * 1.遍历所有需要打包的文件
     * 2.根据文件ID操作该文件
     * 2.根据target移动或更新文件
     * 4.完成所有的文件后打包文件夹
     * 例如
     * 传入参数为 {fileInfos:[{fileId:"1", target:"/身份证/身份证证明.jpg"}],zipName:"ceshi.zip", appKey:"185gs158qje" };将会执行以下操作：
     * 1.移动文件ID为1的文件到 "/身份证/" 目录下(如没有则创建)
     * 2.将移动后的文件名称为 "身份证证明.jpg"
     * 3.连同目录打包文件并将打包后的zip文件命名为 "ceshi.zip"
     * @param fileInfos
     * @param zipName
     * @param appKey
     * @return
     * @throws Exception
     */
    ResultSet<CommonFile> packFilesToZip(List<FileInfo> fileInfos, String zipName, String appKey) throws Exception;

}