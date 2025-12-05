package com.app.backend.repositories;

import com.app.backend.models.KetBan;
import com.app.backend.models.constant.TrangThaiKetBan;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface IKetBanRepository extends JpaRepository<KetBan, Long> {

    boolean existsByNguoiGui_IdAndNguoiNhan_IdAndTrangThai(
            Long nguoiGuiId,
            Long nguoiNhanId,
            TrangThaiKetBan trangThai
    );

    boolean existsByNguoiGui_IdAndNguoiNhan_Id(Long nguoiGuiId, Long nguoiNhanId);

    @Query("""
            SELECT k FROM KetBan k
            WHERE k.nguoiNhan.id = :userId AND k.trangThai = com.app.backend.models.constant.TrangThaiKetBan.PENDING
            """)
    List<KetBan> findIncomingRequests(Long userId);

    @Query("""
            SELECT k FROM KetBan k
            WHERE k.nguoiGui.id = :userId AND k.trangThai = com.app.backend.models.constant.TrangThaiKetBan.PENDING
            """)
    List<KetBan> findOutgoingRequests(Long userId);

    @Query("""
            SELECT k FROM KetBan k
            WHERE (k.nguoiGui.id = :userId OR k.nguoiNhan.id = :userId)
              AND k.trangThai = com.app.backend.models.constant.TrangThaiKetBan.ACCEPTED
            """)
    List<KetBan> findFriends(Long userId);

    Optional<KetBan> findByIdAndNguoiNhan_Id(Long id, Long nguoiNhanId);

    Optional<KetBan> findByIdAndNguoiGui_Id(Long id, Long nguoiGuiId);
}
