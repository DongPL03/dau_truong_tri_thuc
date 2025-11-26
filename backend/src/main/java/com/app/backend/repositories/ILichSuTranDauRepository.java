package com.app.backend.repositories;

import com.app.backend.models.LichSuTranDau;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

public interface ILichSuTranDauRepository extends JpaRepository<LichSuTranDau, Long> {
    void deleteByTranDau_Id(Long tranDauId);

    List<LichSuTranDau> findAllByTranDau_Id(Long tranDauId);

    Page<LichSuTranDau> findByNguoiDung_IdOrderByHoanThanhLucDesc(Long nguoiDungId, Pageable pageable);

    // dùng cho detail
    Optional<LichSuTranDau> findByTranDau_IdAndNguoiDung_Id(Long tranDauId, Long nguoiDungId);

    List<LichSuTranDau> findByTranDau_IdOrderByXepHangAsc(Long tranDauId);

    @Query("""
            SELECT 
              l.nguoiDung.id AS userId,
              SUM(l.tongDiem) AS tongDiem,
              COUNT(l) AS tongTran,
              SUM(CASE WHEN l.xepHang = 1 THEN 1 ELSE 0 END) AS soTranThang
            FROM LichSuTranDau l
            JOIN l.tranDau td
            JOIN td.boCauHoi b
            WHERE (:from IS NULL OR l.hoanThanhLuc >= :from)
              AND (:to IS NULL OR l.hoanThanhLuc < :to)
              AND (:chuDeId IS NULL OR b.chuDe.id = :chuDeId)
              AND (:boCauHoiId IS NULL OR b.id = :boCauHoiId)
            GROUP BY l.nguoiDung.id
            ORDER BY tongDiem DESC
            """)
    Page<LeaderboardAggregateProjection> aggregateLeaderboard(
            @Param("from") Instant from,
            @Param("to") Instant to,
            @Param("chuDeId") Long chuDeId,
            @Param("boCauHoiId") Long boCauHoiId,
            Pageable pageable
    );
}
