package com.app.backend.repositories;

import com.app.backend.models.KhoaHocBoCauHoi;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface IKhoaHocBoCauHoiRepository extends JpaRepository<KhoaHocBoCauHoi, Long> {

    List<KhoaHocBoCauHoi> findByKhoaHocIdOrderByThuTuAsc(Long khoaHocId);

    Optional<KhoaHocBoCauHoi> findByKhoaHocIdAndBoCauHoiId(Long khoaHocId, Long boCauHoiId);

    @Query("""
            SELECT khbch FROM KhoaHocBoCauHoi khbch
            WHERE khbch.khoaHoc.id = :khoaHocId
            AND khbch.thuTu = :thuTu
            """)
    Optional<KhoaHocBoCauHoi> findByKhoaHocIdAndThuTu(@Param("khoaHocId") Long khoaHocId,
                                                       @Param("thuTu") Integer thuTu);

    @Query("""
            SELECT khbch FROM KhoaHocBoCauHoi khbch
            WHERE khbch.khoaHoc.id = :khoaHocId
            AND khbch.thuTu < :thuTu
            ORDER BY khbch.thuTu DESC
            """)
    List<KhoaHocBoCauHoi> findPreviousBoCauHoi(@Param("khoaHocId") Long khoaHocId,
                                                @Param("thuTu") Integer thuTu);

    void deleteByKhoaHocId(Long khoaHocId);
}

