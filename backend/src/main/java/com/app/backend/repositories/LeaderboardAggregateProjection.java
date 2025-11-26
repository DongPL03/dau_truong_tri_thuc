package com.app.backend.repositories;

/**
 * Projection dùng cho BXH tính động từ bảng lich_su_tran_dau
 */
public interface LeaderboardAggregateProjection {

    Long getUserId();

    Long getTongDiem();

    Long getTongTran();

    Long getSoTranThang();
}
