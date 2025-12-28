package com.app.backend.repositories;

import com.app.backend.models.ChanNguoiDung;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface IChanNguoiDungRepository extends JpaRepository<ChanNguoiDung, Long> {

    /**
     * Kiểm tra xem user A đã chặn user B chưa
     */
    boolean existsByNguoiChan_IdAndNguoiBiChan_Id(Long nguoiChanId, Long nguoiBiChanId);

    /**
     * Lấy danh sách những người mà user đã chặn
     */
    @Query("""
            SELECT c FROM ChanNguoiDung c
            WHERE c.nguoiChan.id = :userId
            ORDER BY c.chanLuc DESC
            """)
    List<ChanNguoiDung> findAllByNguoiChan(@Param("userId") Long userId);

    /**
     * Tìm record chặn cụ thể
     */
    Optional<ChanNguoiDung> findByNguoiChan_IdAndNguoiBiChan_Id(Long nguoiChanId, Long nguoiBiChanId);

    /**
     * Kiểm tra xem 2 user có chặn nhau không (bất kỳ chiều nào)
     */
    @Query("""
            SELECT (COUNT(c) > 0) FROM ChanNguoiDung c
            WHERE (c.nguoiChan.id = :u1 AND c.nguoiBiChan.id = :u2)
               OR (c.nguoiChan.id = :u2 AND c.nguoiBiChan.id = :u1)
            """)
    boolean isBlocked(@Param("u1") Long u1, @Param("u2") Long u2);

    /**
     * Lấy danh sách ID những người bị user chặn
     */
    @Query("SELECT c.nguoiBiChan.id FROM ChanNguoiDung c WHERE c.nguoiChan.id = :userId")
    List<Long> findBlockedUserIds(@Param("userId") Long userId);

    /**
     * Lấy danh sách ID những người đã chặn user này
     */
    @Query("SELECT c.nguoiChan.id FROM ChanNguoiDung c WHERE c.nguoiBiChan.id = :userId")
    List<Long> findBlockerUserIds(@Param("userId") Long userId);
}
