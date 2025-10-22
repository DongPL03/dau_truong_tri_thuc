package com.app.backend.repositories;

import com.app.backend.models.TinNhan;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ITinNhanRepository extends JpaRepository<TinNhan, Long> {
}
