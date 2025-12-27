package com.app.backend.repositories;

import com.app.backend.models.BoCauHoi;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface IBoCauHoiRepository extends JpaRepository<BoCauHoi, Long> {
    @Query("""
            SELECT b FROM BoCauHoi b
            WHERE b.isXoa = false
            AND (:keyword IS NULL OR :keyword = '' OR LOWER(b.tieuDe) LIKE %:keyword%)
            AND (:chuDeId IS NULL OR :chuDeId = 0 OR b.chuDe.id = :chuDeId)
            AND (:cheDoHienThi IS NULL OR :cheDoHienThi = '' OR b.cheDoHienThi = :cheDoHienThi)
            AND (:trangThai IS NULL OR :trangThai = '' OR b.trangThai = :trangThai)
            AND (
                 :isAdmin = true
                 OR b.taoBoi.id = :creatorId
                 OR (b.cheDoHienThi = 'PUBLIC' AND b.trangThai = 'DA_DUYET')
            )
            """)
    Page<BoCauHoi> searchBoCauHoi(Pageable pageable,
                                  @Param("keyword") String keyword,
                                  @Param("chuDeId") Long chuDeId,
                                  @Param("cheDoHienThi") String cheDoHienThi,
                                  @Param("trangThai") String trangThai,
                                  @Param("creatorId") Long creatorId,
                                  @Param("isAdmin") boolean isAdmin);

    /**
     * Tìm bộ câu hỏi dành cho practice free (không thuộc khóa học)
     * Loại trừ các bộ câu hỏi thuộc khóa học (check qua khoa_hoc_bo_cau_hoi)
     */
    @Query("""
            SELECT DISTINCT b FROM BoCauHoi b
            WHERE b.isXoa = false
              AND (b.isOfficial = false OR b.isOfficial IS NULL)
              AND b.trangThai = 'DA_DUYET'
              AND (b.loaiSuDung = 'PRACTICE_ONLY' OR b.loaiSuDung = 'BOTH')
              AND NOT EXISTS (
                  SELECT 1 FROM com.app.backend.models.KhoaHocBoCauHoi khbch 
                  WHERE khbch.boCauHoi.id = b.id
              )
              AND (
                    :isAdmin = true
                    OR b.taoBoi.id = :creatorId
                    OR b.cheDoHienThi = 'PUBLIC'
              )
            """)
    Page<BoCauHoi> findPracticeSets(Pageable pageable,
                                    @Param("creatorId") Long creatorId,
                                    @Param("isAdmin") boolean isAdmin);
    
    /**
     * Tìm bộ câu hỏi thuộc một khóa học cụ thể
     */
    @Query("""
            SELECT b FROM BoCauHoi b
            INNER JOIN com.app.backend.models.KhoaHocBoCauHoi khbch ON khbch.boCauHoi.id = b.id
            WHERE b.isXoa = false
              AND b.trangThai = 'DA_DUYET'
              AND khbch.khoaHoc.id = :khoaHocId
            ORDER BY khbch.thuTu ASC
            """)
    List<BoCauHoi> findBoCauHoiByKhoaHocId(@Param("khoaHocId") Long khoaHocId);
    
    /**
     * Kiểm tra xem bộ câu hỏi có thuộc khóa học nào không
     */
    @Query("""
            SELECT COUNT(khbch) > 0 FROM com.app.backend.models.KhoaHocBoCauHoi khbch
            WHERE khbch.boCauHoi.id = :boCauHoiId
            """)
    boolean isBelongToKhoaHoc(@Param("boCauHoiId") Long boCauHoiId);

    @Query("""
            SELECT b FROM BoCauHoi b
            WHERE b.isXoa = false
              AND b.isOfficial = true
              AND b.trangThai = 'DA_DUYET'
              AND b.cheDoHienThi = 'PRIVATE'
            """)
    Page<BoCauHoi> findBattleSets(Pageable pageable);

    @Query("""
            SELECT b FROM BoCauHoi b
            WHERE b.isXoa = false
              AND b.isOfficial = true
              AND b.trangThai = 'DA_DUYET'
              AND (b.loaiSuDung = 'RANKED_ONLY' OR b.loaiSuDung = 'BOTH')
            """)
    List<BoCauHoi> findRankedBattleSets();

    @Query("""
            SELECT b FROM BoCauHoi b
            WHERE b.isXoa = false
              AND b.trangThai = 'DA_DUYET'
              AND (b.loaiSuDung = 'PRACTICE_ONLY' OR b.loaiSuDung = 'BOTH')
              AND (
                    b.isOfficial = true
                 OR b.cheDoHienThi = 'PUBLIC'
                 OR b.taoBoi.id = :userId
              )
            """)
    List<BoCauHoi> findCasualBattleSets(Long userId);


    @Override
    long count();

    long countByTrangThai(String trangThai);

}
