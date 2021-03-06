package com.github.misterchangray.dao.mapper;

import com.github.misterchangray.dao.entity.CommonFile;
import com.github.misterchangray.dao.entity.CommonFileQuery;
import java.util.List;
import org.apache.ibatis.annotations.Param;

public interface CommonFileMapper {
    long countByQuery(CommonFileQuery query);

    int deleteByQuery(CommonFileQuery query);

    int insert(CommonFile record);

    int insertSelective(CommonFile record);

    List<CommonFile> selectByQuery(CommonFileQuery query);

    int updateByQuerySelective(@Param("record") CommonFile record, @Param("example") CommonFileQuery query);

    int updateByQuery(@Param("record") CommonFile record, @Param("example") CommonFileQuery query);

    /**
     * This method was generated by MyBatis Generator.
     * This method corresponds to the database table common_file
     *
     * @mbg.generated
     * @project https://github.com/itfsw/mybatis-generator-plugin
     */
    int batchInsert(@Param("list") List<CommonFile> list);

    /**
     * This method was generated by MyBatis Generator.
     * This method corresponds to the database table common_file
     *
     * @mbg.generated
     * @project https://github.com/itfsw/mybatis-generator-plugin
     */
    int batchInsertSelective(@Param("list") List<CommonFile> list, @Param("selective") CommonFile.Column ... selective);
}