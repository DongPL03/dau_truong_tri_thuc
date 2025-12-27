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

    Page<LichSuTranDau> findAllByOrderByHoanThanhLucDesc(Pageable pageable);

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

    /**
     * Tìm kiếm lịch sử trận đấu với filter nâng cao
     */
    @Query("""
            SELECT l FROM LichSuTranDau l
            JOIN l.tranDau td
            JOIN td.boCauHoi b
            JOIN l.nguoiDung nd
            WHERE (:keyword IS NULL OR :keyword = '' 
                   OR LOWER(td.tenPhong) LIKE LOWER(CONCAT('%', :keyword, '%'))
                   OR LOWER(b.tieuDe) LIKE LOWER(CONCAT('%', :keyword, '%'))
                   OR LOWER(nd.hoTen) LIKE LOWER(CONCAT('%', :keyword, '%'))
                   OR LOWER(nd.tenDangNhap) LIKE LOWER(CONCAT('%', :keyword, '%')))
              AND (:loaiTranDau IS NULL OR :loaiTranDau = '' OR td.loaiTranDau = :loaiTranDau)
              AND (:boCauHoiId IS NULL OR b.id = :boCauHoiId)
              AND (:from IS NULL OR l.hoanThanhLuc >= :from)
              AND (:to IS NULL OR l.hoanThanhLuc <= :to)
            ORDER BY l.hoanThanhLuc DESC
            """)
    Page<LichSuTranDau> findAllFiltered(
            @Param("keyword") String keyword,
            @Param("loaiTranDau") String loaiTranDau,
            @Param("boCauHoiId") Long boCauHoiId,
            @Param("from") Instant from,
            @Param("to") Instant to,
            Pageable pageable
    );

    /**
     * Tìm kiếm lịch sử trận đấu với filter (không phân trang - cho export)
     */
    @Query("""
            SELECT l FROM LichSuTranDau l
            JOIN l.tranDau td
            JOIN td.boCauHoi b
            JOIN l.nguoiDung nd
            WHERE (:keyword IS NULL OR :keyword = '' 
                   OR LOWER(td.tenPhong) LIKE LOWER(CONCAT('%', :keyword, '%'))
                   OR LOWER(b.tieuDe) LIKE LOWER(CONCAT('%', :keyword, '%'))
                   OR LOWER(nd.hoTen) LIKE LOWER(CONCAT('%', :keyword, '%'))
                   OR LOWER(nd.tenDangNhap) LIKE LOWER(CONCAT('%', :keyword, '%')))
              AND (:loaiTranDau IS NULL OR :loaiTranDau = '' OR td.loaiTranDau = :loaiTranDau)
              AND (:boCauHoiId IS NULL OR b.id = :boCauHoiId)
              AND (:from IS NULL OR l.hoanThanhLuc >= :from)
              AND (:to IS NULL OR l.hoanThanhLuc <= :to)
            ORDER BY l.hoanThanhLuc DESC
            """)
    List<LichSuTranDau> findAllFilteredList(
            @Param("keyword") String keyword,
            @Param("loaiTranDau") String loaiTranDau,
            @Param("boCauHoiId") Long boCauHoiId,
            @Param("from") Instant from,
            @Param("to") Instant to
    );
}
