package com.app.backend.repositories;

import com.app.backend.models.KetBan;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IKetBanRepository extends JpaRepository<KetBan, Long> {
}
