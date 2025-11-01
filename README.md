# 🤝 Social Trust Score System

A decentralized social trust scoring system built on Stacks blockchain that enables on-chain reputation management for governance participation and service access control.

## 🌟 Features

- **Trust Score Management**: Users build trust scores through peer endorsements
- **Service Access Control**: Gate services behind minimum trust requirements  
- **Governance Voting**: Weighted voting based on trust scores and reputation
- **Reputation Levels**: Progressive reputation system based on endorsement history
- **User Verification**: Admin verification system for enhanced credibility
- **Anti-Gaming Mechanisms**: Cooldown periods and self-endorsement prevention

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd social-trust-score
clarinet check
```

## 📖 Usage Guide

### 🆕 Initialize Your Profile

Before using the system, initialize your user profile:

```clarity
(contract-call? .social-trust-score initialize-user)
```

### 👍 Endorse Other Users

Endorse users to boost their trust scores (requires minimum trust score):

```clarity
(contract-call? .social-trust-score endorse-user 'SP1234... u5 "reliability")
```

Parameters:
- `endorsed`: Principal of user to endorse
- `weight`: Endorsement weight (1-10)  
- `category`: Endorsement category (max 20 chars)

### 🚨 Report Problematic Users

Report users to reduce their trust scores:

```clarity
(contract-call? .social-trust-score report-user 'SP1234...)
```

### 🔐 Create Service Requirements

Define trust requirements for your service:

```clarity
(contract-call? .social-trust-score create-service-requirement 
  "premium-access" u75 u5 true)
```

Parameters:
- `service-id`: Unique service identifier
- `min-trust-score`: Minimum required trust score
- `min-endorsements`: Minimum endorsement count
- `require-verification`: Whether verification is required

### 🎫 Request Service Access

Request access to a service (automatically checks requirements):

```clarity
(contract-call? .social-trust-score request-service-access "premium-access")
```

### 🗳️ Participate in Governance

Cast weighted votes in governance proposals:

```clarity
(contract-call? .social-trust-score cast-governance-vote u1 true)
```

## 📊 Read-Only Functions

### Check Trust Score
```clarity
(contract-call? .social-trust-score get-trust-score 'SP1234...)
```

### Check Service Access Eligibility
```clarity
(contract-call? .social-trust-score can-access-service 'SP1234... "service-name")
```

### Get Voting Weight
```clarity
(contract-call? .social-trust-score get-voting-weight 'SP1234...)
```

### View User Profile
```clarity
(contract-call? .social-trust-score get-user-profile 'SP1234...)
```

## ⚙️ System Parameters

- **Initial Trust Score**: 50 points
- **Maximum Trust Score**: 100 points  
- **Endorsement Cooldown**: 144 blocks (~24 hours)
- **Reputation Levels**: Based on endorsement count (every 10 endors
