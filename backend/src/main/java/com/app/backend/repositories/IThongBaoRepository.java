package com.app.backend.repositories;

import com.app.backend.models.ThongBao;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface IThongBaoRepository extends JpaRepository<ThongBao, Long> {

    Page<ThongBao> findByNguoiNhan_IdOrderByTaoLucDesc(Long nguoiNhanId, Pageable pageable);

    long countByNguoiNhan_IdAndDaDocFalse(Long nguoiNhanId);

    @Modifying
    @Query("""
            UPDATE ThongBao t
            SET t.daDoc = TRUE
            WHERE t.id = :thongBaoId AND t.nguoiNhan.id = :nguoiNhanId
            """)
    int markReadByIdAndNguoiNhan(Long thongBaoId, Long nguoiNhanId);

    @Modifying
    @Query("""
            UPDATE ThongBao t
            SET t.daDoc = TRUE
            WHERE t.nguoiNhan.id = :nguoiNhanId AND t.daDoc = FALSE
            """)
    int markAllReadByNguoiNhan(Long nguoiNhanId);
}

