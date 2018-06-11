package com.github.misterchangray.service.file;

import com.github.misterchangray.common.ResultSet;
import com.github.misterchangray.dao.entity.CommonFile;
import com.github.misterchangray.service.BaseService;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

/**
 * 文件服务
 *
 * @author Rui.Zhang/misterchangray@hotmail.com
 * @author Created on 3/20/2018.
 */
public interface FileService extends BaseService<CommonFile> {
    ResultSet<CommonFile> saveFile(MultipartFile uploadFile, String appKey) throws IOException;
}