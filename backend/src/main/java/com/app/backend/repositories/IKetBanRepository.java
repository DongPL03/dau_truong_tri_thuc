package com.app.backend.repositories;

import com.app.backend.models.KetBan;
import com.app.backend.models.constant.TrangThaiKetBan;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface IKetBanRepository extends JpaRepository<KetBan, Long> {

    boolean existsByNguoiGui_IdAndNguoiNhan_IdAndTrangThai(
            Long nguoiGuiId,
            Long nguoiNhanId,
            TrangThaiKetBan trangThai
    );

    boolean existsByNguoiGui_IdAndNguoiNhan_Id(Long nguoiGuiId, Long nguoiNhanId);

    /**
     * Lời mời đến (user là người nhận, trạng thái PENDING)
     */
    @Query("""
            SELECT k FROM KetBan k
            WHERE k.nguoiNhan.id = :userId
              AND k.trangThai = com.app.backend.models.constant.TrangThaiKetBan.PENDING
            ORDER BY k.taoLuc DESC
            """)
    List<KetBan> findIncomingRequests(@Param("userId") Long userId);

    /**
     * Lời mời đã gửi (user là người gửi, trạng thái PENDING)
     */
    @Query("""
            SELECT k FROM KetBan k
            WHERE k.nguoiGui.id = :userId
              AND k.trangThai = com.app.backend.models.constant.TrangThaiKetBan.PENDING
            ORDER BY k.taoLuc DESC
            """)
    List<KetBan> findOutgoingRequests(@Param("userId") Long userId);

    @Query("""
            SELECT k FROM KetBan k
            WHERE (k.nguoiGui.id = :userId OR k.nguoiNhan.id = :userId)
              AND k.trangThai = com.app.backend.models.constant.TrangThaiKetBan.ACCEPTED
            """)
    List<KetBan> findFriends(Long userId);

    Optional<KetBan> findByIdAndNguoiNhan_Id(Long id, Long nguoiNhanId);

    Optional<KetBan> findByIdAndNguoiGui_Id(Long id, Long nguoiGuiId);

    /**
     * ✅ Check 2 user đã là bạn bè chưa
     */
    @Query("""
            SELECT (COUNT(k) > 0) FROM KetBan k
            WHERE k.trangThai = com.app.backend.models.constant.TrangThaiKetBan.ACCEPTED
              AND (
                (k.nguoiGui.id = :u1 AND k.nguoiNhan.id = :u2)
                OR
                (k.nguoiGui.id = :u2 AND k.nguoiNhan.id = :u1)
              )
            """)
    boolean areFriends(@Param("u1") Long u1, @Param("u2") Long u2);
}



