package com.app.backend.repositories;

import com.app.backend.models.TraLoiTranDau;
import com.app.backend.responses.thongke.TopicStatResponse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ITraLoiTranDauRepository extends JpaRepository<TraLoiTranDau, Long> {
    List<TraLoiTranDau> findAllByTranDau_Id(Long tranDauId);

    List<TraLoiTranDau> findByTranDau_IdAndNguoiDung_IdOrderByTraLoiLucAsc(Long tranDauId, Long nguoiDungId);

    List<TraLoiTranDau> findByTranDau_IdAndCauHoi_IdOrderByTraLoiLucAsc(
            Long tranDauId, Long cauHoiId);

    @Query("""
            SELECT new com.app.backend.responses.thongke.TopicStatResponse(
              cd.id,
              cd.ten,
              COUNT(tl.id),
              SUM(CASE WHEN tl.dungHaySai = true THEN 1 ELSE 0 END)
            )
            FROM TraLoiTranDau tl
            JOIN tl.cauHoi ch
            JOIN ch.boCauHoi.chuDe cd
            WHERE tl.nguoiDung.id = :userId
            GROUP BY cd.id, cd.ten
            """)
    List<TopicStatResponse> getTopicStatsRawByUser(@Param("userId") Long userId);

    /**
     * Xóa các câu trả lời theo tranDau ID và nguoiDung ID
     */
    void deleteByTranDau_IdAndNguoiDung_Id(Long tranDauId, Long nguoiDungId);
}

