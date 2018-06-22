package com.github.misterchangray.service.file;

import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.dao.entity.CommonFile;
import com.github.misterchangray.service.BaseService;
import com.github.misterchangray.service.file.dto.FileInfo;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * 文件服务
 *
 * @author Rui.Zhang/misterchangray@hotmail.com
 * @author Created on 3/20/2018.
 */
public interface FileService {
    ResultSet<CommonFile> saveFile(MultipartFile uploadFile, String appKey) throws IOException;



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
    ResultSet<CommonFile> packFilesToZip(List<FileInfo> fileInfos, String zipName, String appKey) throws Exception;

    /**
     * 获取文件实例
     * @param fileId
     * @return
     */
    ResultSet<File> getFile(String fileId);

    /**
     *
     * @param fileId
     * @param appKey
     * @return
     */
    ResultSet buildDownloadUrl(String fileId, String appKey);

    boolean validToken(String token);
    boolean existAppKey(String appKey);
}