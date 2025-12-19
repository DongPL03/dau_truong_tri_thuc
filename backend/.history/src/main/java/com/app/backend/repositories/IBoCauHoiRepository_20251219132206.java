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

    @Query("""
            SELECT b FROM BoCauHoi b
            WHERE b.isXoa = false
              AND (b.isOfficial = false OR b.isOfficial IS NULL)
              AND b.trangThai = 'DA_DUYET'
              AND (
                    :isAdmin = true
                    OR b.taoBoi.id = :creatorId
                    OR b.cheDoHienThi = 'PUBLIC'
              )
            """)
    Page<BoCauHoi> findPracticeSets(Pageable pageable,
                                    @Param("creatorId") Long creatorId,
                                    @Param("isAdmin") boolean isAdmin);

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
